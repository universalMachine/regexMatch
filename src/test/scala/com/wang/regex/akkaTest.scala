package com.wang.regex



import akka.actor.{Actor, ActorContext, ActorRef, ActorSystem, Props}
import org.scalatest.{FunSuite, Matchers}

class Ping extends Actor{
  override def receive: Receive = {
    case default => println(default);sender() ! 5
  }
}

class Pong extends Actor{
  override def receive: Receive = {
    case actorRef:ActorRef => actorRef ! "e"
    case x => println(x);sender() ! 7
  }
}


class akkaTest extends FunSuite with Matchers{


    test("create akka actor") {
      val system =ActorSystem("actor-system")
      val ping = system.actorOf(Props[Ping],"ping")
      val pong = system.actorOf(Props[Pong],"pong")

      pong! ping
    }


}
