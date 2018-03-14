package com.wang.regex

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

import akka.actor
import akka.actor.{Actor, ActorSystem, Props}
import helper.logger
object Nonblocking {

  trait Future[+A] {
    private[regex] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new java.util.concurrent.atomic.AtomicReference[A] // A mutable, threadsafe reference, to use for storing the result
      val latch = new CountDownLatch(1) // A latch which, when decremented, implies that `ref` has the result
      p(es) { a => ref.set(a); latch.countDown } // Asynchronously set the result, and decrement the latch
      latch.await // Block until the `latch.countDown` is invoked asynchronously
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


    def map[A,B](a:Par[A])(f: A=>B):Par[B] = fork(map2(a,unit()){case (a,_) => f(a)})

    /**
      * Helper function, for evaluating an action
      * asynchronously, using the given `ExecutorService`.
      */
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = {logger.debug("nonblock eval");r} })

    def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None
          // this implementation is a little too liberal in forking of threads -
          // it forks a new logical thread for the actor and for stack-safety,
          // forks evaluation of the callback `cb`

          val system = ActorSystem("combiner")
          object CombinerActor {

            def props(): Props = actor.Props(new CombinerActor)
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
      }


    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def asyncF[A,B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))



    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l,r) = as.splitAt(as.length/2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] =
      sequence(as.map(asyncF(f)))




  }
}
