package com.wang.regex

import org.scalatest.{FunSpec, Matchers}

class SequenceTest extends FunSpec with Matchers {
  val repeatc = Repeat(Literal('c'))
  val sequence = Sequence(repeatc,Literal('d'))
  describe("input contains"){
    it("shoud output many streams"){
      sequence.regexMatch(List("ccca","d1a")) should contain theSameElementsAs List("ccca","da","cca","ca","a")
    }
  }
}
