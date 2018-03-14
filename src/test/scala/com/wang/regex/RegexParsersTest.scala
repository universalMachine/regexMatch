package com.wang.regex
import RegexParsers._
import com.wang.regex.Parser.ParserState
import com.wang.regex.errorHandle.{Location, ParseError}
import org.scalatest.{FunSuite, Matchers}

class RegexParsersTest extends FunSuite with  Matchers{
  test("testStringFunc"){
    val loc = Location("123456",1)
    ParserState(loc).input should be("23456")
  }

  test("stringParser"){

   println( RegexParsers.run("34" & scope("first",
     attempt(scope("first error","345")) | scope("next","3fdaf")))("63434535") match {
     case ParseFail(error,_) => error
     case result => result
   })

    RegexParsers.run("3fdaf")("3434535") match {
      case ParseFail(error,_) => error.latestLoc.get.offset should be(1)
    }
   println( RegexParsers.run("4" )("3333477") match {
      case ParseFail(error,_) => error.latestLoc.get.offset should be(1)
      case result=> result
    })

    //RegexParsers.run("")
  }
}
