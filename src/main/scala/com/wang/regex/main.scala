package com.wang.regex
import helper.logger
object MyModule{
  def main(args: Array[String]): Unit = {
    logger.trace("logback trace success")
    println("a".substring(1));
    Literal('c').regexMatch(List("c","cg"))
  }
}

