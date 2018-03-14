package com.wang.regex

import scala.io.StdIn

trait IO[A] {self=>
  def run: A

  def ++[B](io: IO[B]) = {
    self.run;
    io.run
  }

  def map[B](f: A => B): IO[B] = new IO[B] { def run = f(self.run) }

  def flatMap[B](f: A=> IO[B]):IO[B] = new IO[B] {
    override def run  = f(self.run).run
  }
}

object IO {
  def PrintLine(msg: String): IO[Unit] = new IO[Unit] { def run = println(msg) }
  //def empty[A]: IO[A] = new IO[A] { def run = {} }
  def unit[A](a: A):IO[A] = new IO[A] {
    override def run = a
  }
  def apply[A](a: =>A):IO[A] = unit(a)
  def ReadLine: IO[String] = IO { StdIn.readLine() }

  def fileStream(filename: String):IO[Stream[String]] = IO {
    val src = io.Source.fromFile(filename)
    src.getLines().toStream.append {src.close();Stream.empty}
  }


}
