package com.wang.regex
import java.io.File
import java.net.URL
import java.nio.file.Files
import java.nio.file.FileSystems

import com.wang.regex.Process._
import java.util.function.Consumer

import callbackPar._
import com.wang.regex.ParHelper.Par

import scala.reflect.io.{Directory, Path}
import helper._

import scala.annotation.tailrec
/*import com.wang.regex

sealed trait Process2[I,O]{
  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt(END) => Stream()
    case Await(req,recv) => req match{
      case h #::t => recv(Some(h))(t)
      case xs => recv(None)(xs)
    }
    case Emit(h,t) => logger.trace(Emit(h,t).toString);h #:: t(s)
  }
  /*def repeat: Process[I,O] = {
    def go(p:Process[I,O]): Process[I,O] = p match {
      case Halt(END) => go(this)
      case Emit(h,t) => {logger.debug(Emit(h,t).toString);Emit(h,go(t))}
      case Await(req,recv) => Await(req.tail,{
        case Some(i)=> go(recv(Some(i)))
        case None => recv(None)
      })
    }
    go(this)
  }*/

/*
  def |>[O2] (p2: Process[O,O2]):Process[I,O2] ={
    def go():Process[I,O2] = Await({
      case Some(i) => Emit(p2(this(Stream(i))).head, go())
      case None => Halt(END)
    })
    go()

  }*/

  def onHalt(f: Throwable=> Process[I,O]): Process[I,O] = this match{
    case Halt(e) => Try(f(e))
    case Emit(h,t) => Emit(h,t.onHalt(f))
    case Await(req,g) => Await(req,(g.andThen(_.onHalt(f))))
  }




  def Try[F,O](p: =>Process[F,O]): Process[F,O] ={
    try p
    catch {case e: Throwable => Halt(e)}
  }

  def ++(p: Process[I,O]):Process[I,O] = this.onHalt{
    case END => p
    case e => p ++ Halt(e)
  }

  def repeat: Process[I,O] =
    this ++ this.repeat

  def onComplete(p: Process[I,O]) = this.onHalt{
    case END => p.asFinal
    case e => p.asFinal ++ Halt(e)
  }

  def asFinal:Process[I,O] = this match {
    case Emit(h,t) => Emit(h,t.asFinal)
    case Halt(e) => Halt(e)
    case Await(req,recv) => Await(req,{
      //assure not be killed
      case Some(a) => recv(Some(a))//a match { case KILL=> this.asFinal case _ => recv(Some(a))}
      case None => recv(None)
    })
  }


  //def map[O2](f: O=>O2):Process[I,O2] = this |> Process.lift(f)


}

case class Emit[I,O](head: O,tail: Process[I,O] = Halt[I,O](END)) extends Process[I,O]

case class Halt[I,O](e: Throwable) extends Process[I,O]

case class Await[I,O](req:Stream[I],recv: Option[I] => Process[I,O]) extends Process[I,O]



object Process2{
 /* def liftOne[I,O](f: I=>O):Process[I,O] = Await{
    case Some(a) => Emit(f(a),Halt(END))
    case None => Halt(END)
  }

  def lift[I,O](f: I=>O) = Process.liftOne(f).repeat

  def filter[I](p: I=>Boolean):Process[I,I] = Await{
    case Some(i) if(p(i)) =>   Emit(i)
    case _ => Halt(END)
  }
*/
 // def sum:Process[Double,Double]

  def eval[I,O](p:Process[I,O]):Stream[O]=p match {
    case Emit(h,t:Process[I,O]) => h #:: eval(t)
    case Halt(e) => Stream()
    case Await(req,recv) => req match {
      case h#::tail => recv(Some(h)) match {
        case Await(req,recv) => eval(Await(tail,recv))
        case x=> eval(x)
      }
      case xs=>eval(recv(None))
    }
  }
}*/
case object END extends Exception

case object KILL extends Exception

object find{
  /*def pathToStream(dirname:String):Stream[Path] =  try{
    //Path(dirname).

    } catch {case e:Exception => println(e.getMessage);Stream.empty}*/
  def Try[I,O](p: Process[I,O]): Process[I,O] ={
    try p
    catch { case e:Throwable=> Halt[I,O]()}
  }

 def findMoreThan(dirname:String,minSize: Int):List[(Path,Long)]={
    def findMoreThanEveryLevel(acc: List[(Path,Long)],list: List[Path]): List[(Path,Long)] ={
      val levelList = list.filterNot(p=>p.isFile&&p.length < minSize)
        .toList.groupBy(_.isFile)
      val files = levelList.get(true).getOrElse(List.empty)
      val filesWithLength = files.map(file=>(file,file.length))
      val directorys = levelList.get(false)
      logger.debug(acc.toString())
      directorys match {
        case Some(dirs) =>findMoreThanEveryLevel(filesWithLength ++ acc,dirs.flatMap(dir=>Directory(dir).list))
        case None => filesWithLength
      }
    }

    findMoreThanEveryLevel(List.empty,Directory(dirname).list.toList).toList
  }

  def ParFindMoreThan(dirname:String,minSize: Int):Par[List[(Path,Long)]]={
    
    def findMoreThanEveryLevel(acc: Par[List[Path]],list: Par[List[Path]]): Par[List[Path]] ={
      val levelList = list.flatMap(ls =>parFoldBalance(ls.toIndexedSeq)(p=>if(p.isFile&&p.isFile&&p.length < minSize) List.empty else List(p))
      ((lp1,lp2)=>lp1:::lp2).flatMap(ls=> lazyUnit(ls.toList.groupBy(_.isFile))))
      val parFiles = levelList.map(_.get(true).getOrElse(List.empty))
      //val filesWithLength = parParmap(parFiles)(file=>(file,file.length))
      val directorys = levelList.map(_.get(false))
      logger.debug(acc.toString())
      directorys.flatMap(parDirs=> parDirs match {
        case Some(dirs) => findMoreThanEveryLevel(map2(parFiles,acc)(_++_),flatmapPar(dirs)(dir=>Directory(dir).list.toList))
        case None => parFiles
      })

    }

    parParmap(findMoreThanEveryLevel(unit(List.empty),unit(Directory(dirname).list.toList)))(path=>(path,path.length)).map(_.toList)
  }
}

