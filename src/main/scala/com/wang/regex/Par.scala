package com.wang.regex

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent._

import ParHelper._
import akka.actor.{Actor, ActorSystem, Props}

import helper.logger


trait Future[+A] {
  private[regex] def apply(k: A => Unit): Unit
}


object ParHelper{
  type Par[A] = ExecutorService => Future[A]
}

trait Pars{
  def unit[A](a: A):Par[A]
  def run[A](es: ExecutorService)(a: Par[A]):A
  def fork[A](a: Par[A]): Par[A]
  def map2[A,B,C](a:Par[A],b:Par[B])(f:(A,B)=>C):Par[C]

  def map[A,B](a:Par[A])(f: A=>B):Par[B] = fork(map2(a,unit(3)){case (a,_) => f(a)})

  def lazyUnit[A](a: =>A):Par[A] = {fork(unit(a))}

  def asyncF[A,B](f:A=>B): A => Par[B] = a => lazyUnit(f(a))


  def travase[A,B](as:List[A])(f: A=>Par[B]): Par[List[B]] = as.foldRight(unit(List[B]()))((a,foldedVal) => map2(f(a),foldedVal)(_::_))
}




trait  callbackFuture[A]{
  def apply(cb: A=>Unit):Unit
}



object callbackPar { self=>
 /* def unit[A](a: A): Par[A] = es => new Future[A] {
    override def apply(cb: A => Unit): Unit = cb(a)
  }

   def run[A](es: ExecutorService)(a: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    a(es).apply(a=> {ref.set(a);latch.countDown})
     latch.await
    ref.get()
  }



   def fork[A](a: Par[A]): Par[A] = es => new Future[A] {
    override def apply(cb: A => Unit): Unit = {
      eval(es)(a(es)(cb))
    }

  }*/
 // def parGroupBy[A](a: List[A])(f: (A,A)=> A ):Par[List[A]] = parFoldBalance(a.toIndexedSeq)(element=>List(element))((b1,b2)=>
   /* for{
      b1r <- b1
      b2r <- b2
    }yield f(b1r,b2r)*/
    /*b1.flatMap(b1r=> b2.filter(b2r=>b1r._2 ==b1r._2))*/

 implicit def operators[A](p: Par[A]) = parOps[A](p)

  case class parOps[A](p:Par[A]){
    def map[B](f: A=>B): Par[B] = self.map(p)(f)
    def run(es:ExecutorService) = self.run(es)(p)
    def flatMap[B](f: A=>Par[B]):Par[B] = self.flatMap(p)(f)

  }
  //注意：有可能造成死锁
  def run[A](es: ExecutorService)(p: Par[A]): A = {
    logger.debug("start run")
    val ref = new java.util.concurrent.atomic.AtomicReference[A] // A mutable, threadsafe reference, to use for storing the result
    val latch = new CountDownLatch(1) // A latch which, when decremented, implies that `ref` has the result
    eval(es)(p(es) { a => ref.set(a);latch.countDown; } )// Asynchronously set the result, and decrement the latch
    logger.debug("blocking")
    latch.await // Block until the `latch.countDown` is invoked asynchronously
    logger.debug("latch unlock")
    logger.debug(s"run callback;run ${ref.get()},count:${latch.getCount}");
    ref.get // Once we've passed the latch, we know `ref` has been set, and return its value


  }

  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
         cb(a)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        eval(es)(a(es)(cb))
    }

 // def map[A,B](a:Par[A])(f: A=>B):Par[B] = map2(a,unit()){case (a,_) => f(a)}

  // specialized version of `map`
  def map[A,B](p: Par[A])(f: A => B): Par[B] =
    es => new Future[B] {
      def apply(cb: B => Unit): Unit = {
        logger.trace("apply map");
        eval(es)(p(es) { result => cb(f(result)) })
      }
    }

  def lazyUnit[A](a: =>A):Par[A] = {fork(unit(a))}

  def asyncF[A,B](f:A=>B): A => Par[B] = a => lazyUnit(f(a))

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = {logger.trace("bar eval");r} })

  def ParMap[A,B](as: List[A])(f: A=>B ):Par[List[B]] = {sequence(as.map(asyncF(f)))(a1=>IndexedSeq(a1),(a1,a2)=>IndexedSeq(a1,a2))}
  def ParFlatMap[A,B](as: List[A])(f: A=>Par[B] ):Par[List[B]] = sequence(as.map(f))(a1=>IndexedSeq(a1),(a1,a2)=>IndexedSeq(a1,a2))

  def flatmapPar[A,B](as:List[A])(f:A=>List[B]):Par[List[B]] = sequence(as.map(asyncF(f)))(b1=>b1.toIndexedSeq,(b1,b2)=>(b1:::b2).toIndexedSeq)


  def parParmap[A,B](parAs: Par[List[A]])(f:A=>B):Par[List[B]] = parAs.flatMap(ParMap(_)(f))

 def parParflatmap[A,B](parAs: Par[List[A]])(f:A=>Par[B]):Par[List[B]] = parAs.flatMap(ParFlatMap(_)(f))

  def sequence[A,B](as: List[Par[A]])(g:A=>IndexedSeq[B],f:(A,A)=>IndexedSeq[B]):Par[List[B]] ={
    //logger.debug(as.toString());
    //as.foldLeft(unit(List[A]()))((accum,a) => map2(a,accum)(_::_))
     map(sequenceBalanced(as.toIndexedSeq)(g)(f))(_.toList)
  }

  def sequenceBalanced[A,B](aseq: IndexedSeq[Par[A]])(g:A=>IndexedSeq[B])(f:(A,A)=>IndexedSeq[B]):Par[IndexedSeq[B]] = {

    if(aseq.size == 0){
      unit(IndexedSeq.empty[B])
    }else if(aseq.size <= 2){
      if(aseq.size == 1)
        map(aseq.head)(g(_))
      else
        map2(aseq.head,aseq.tail.head)(f(_,_))
    }else{
      val (l,r) = aseq.splitAt(aseq.length/2)

      (map2(sequenceBalanced(l)(g)(f),sequenceBalanced(r)(g)(f))(_++_))
    }
  }


  def flatMap[A,B](a: Par[A])(f: A=>Par[B]):Par[B] = {logger.trace("start flatmap");join(a.map(f))}

  def join[A](p: Par[Par[A]]): Par[A] = es=> new Future[A]{
    override private[regex] def apply(cb: A => Unit): Unit = {
      //logger.debug(s"join");
      //val p2 =p.map(p2=>{logger.debug("join map callback");run(es)(p2)})
      //logger.debug("after call back")
       //cb(p2.run(es))
      p.map(_(es)(a=>cb(a)))(es)(a=>a)
    }

  }

  def foldBalance[A,B](s: IndexedSeq[A])(f: A=>B)(g: (B,B)=>B):B =
    if(s.size == 2){
      g(f(s.head),f(s.tail.head))
  } else if(s.size == 1) {
    f(s.head)
  }else{
      s.splitAt(s.size/2) match{
        case (l,r) => g(foldBalance(l)(f)(g),foldBalance(r)(f)(g))
      }
    }

  def parFoldBalance[A,B](s: IndexedSeq[A])(f: A=>B)(g: (B,B)=>B):Par[B] = {
    logger.debug(s"seq:$s")
  if(s.size <10) {
       unit(foldBalance(s)(f)(g))
   }else

      flatMap(unit(s.splitAt(s.size/2)).map{case (l,r)=> {logger.trace(s"L:$l;R:$r");(fork(parFoldBalance(l)(f)(g)),fork((parFoldBalance(r)(f)(g))))}})
       {case (parl,parR)=>logger.debug(s"flatmap $parl;$parR");
         map2(parl,parR){case (l,r)=>logger.debug(s"l:$l;r:$r");g(l,r)}}
  }






   def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => new Future[C] {

    override def apply(cb: C => Unit): Unit = {
      var ar: Option[A] = None
      var br: Option[B] = None

     val combiner = selfActor[Either[A,B]](es) {

        case Left(a) =>
          logger.debug(s"map2 a:$a,b:$br");
          if (br.isDefined) eval(es)(cb(f(a,br.get)))
          else ar = Some(a)
        case Right(b) =>
          logger.debug(s"map2 a:$ar,b:$b");
          if (ar.isDefined) eval(es)(cb(f(ar.get,b)))
          else br = Some(b)
      }

      a(es).apply(result=>combiner ! Left(result))
      b(es).apply(result=>combiner ! Right(result))
    }
  }
  /*override def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] =
    es => new callbackFuture[C] {
      def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None
        // this implementation is a little too liberal in forking of threads -
        // it forks a new logical thread for the actor and for stack-safety,
        // forks evaluation of the callback `cb`

        val system = ActorSystem("combiner")
        object CombinerActor {

          def props(): Props = Props(new CombinerActor)
        }
        class CombinerActor extends  Actor{
          override def receive: Receive = {
            case Left(a: A) => br match{
              case None => ar = Some(a)
              case Some(b) => cb(f(a,b))
            }

            case Right(b: B) => ar match{
              case None => br = Some(b)
              case Some(a) => cb(f(a,b))
            }
          }
        }
        val combiner = system.actorOf(CombinerActor.props());


        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }*/

  //凡是Parser[A]都可以使用ParserOps

}

