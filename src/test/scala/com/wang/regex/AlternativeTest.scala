package com.wang.regex

import org.scalatest.{FunSpec, FunSuite}

class AlternativeTest extends FunSpec {
  val alternative = Alternative(Literal('o'),Literal('c'))
  val alternative2 = Alternative(Literal('c'),alternative)
  describe("when input is Nil"){
    it("should output Nil"){
      assert(alternative.regexMatch(Nil) == Nil)
      assert(alternative2.regexMatch(Nil) == Nil)

    }
  }

  describe("when input is contains"){
    it("should output two stream when all hit"){
      assert(alternative2.regexMatch(List("cbd")) == List("bd","bd"))
    }
    it("should output two stream when all hit one by one"){
      assert(alternative2.regexMatch(List("o","cb")) == List("","","b"))
    }
  }

  describe("when input is not contains"){
    it("should output one Nil"){
      assert(alternative2.regexMatch(List("2313","falfjaf")) == Nil)
    }
  }
}
