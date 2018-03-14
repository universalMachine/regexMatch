package com.wang.regex

import java.util.concurrent.Executors

import org.scalatest.FunSuite

import scala.reflect.io.{Directory, File}

class ProcessTest extends FunSuite {
 /* val process:Process[Int,Int] = Process.liftOne(a=>a*2)
  val process2:Process[Int,Int] = Process.liftOne(a=>a+100)
  val repeatProcess: Process[Int,Int] = Process.lift(a=>a*2)*/
  val stream = Stream.range(2,20)
/*
  test("testRepeat") {
    println(repeatProcess(Stream.range(2,7)).toList)
  }

  test("testFilter"){
    val even = Process.filter((x: Int)=>(x % 2 == 0))
    println(even.repeat(stream).toList)
  }

  test("testPipe"){
   // println((process |> process2 ).repeat(stream).toList)
  }

  test("testMap"){
    println(process.map(a=>a+10)(stream).toList)
  }

  test("testOnHalt"){
    IO.PrintLine(process.onComplete(Emit(999,Halt(END))).repeat(stream).toList
      .toString).run
  }
*/
val es = Executors.newFixedThreadPool(40)
  test("testFind"){
  /*  println(Process.eval(Await[Int,Int](stream,{
      case Some(i) => Emit(i*3)
      case None=>Halt(END)
    }).repeat).toList.toString())*/
    //println(find.pathToStream()(Stream("/home")))
   // println(find.findMoreThan(".",100))
    println(callbackPar.run(es)(find.ParFindMoreThan("/home/extend" +
      "",100000000)))
    /*val levelList = Directory("/home").list.filterNot(p=>p.isFile&&p.length<1)
      .toList.groupBy(_.isFile)*/
      //.flatMap{case (true,paths:List[Directory])=>paths.map(_.list.toList)
    //case (false,files)=>files.map(file=>(file,file.length))}

    /*val files = levelList.get(true).getOrElse(List.empty)
    println(files)*/
  }
}
