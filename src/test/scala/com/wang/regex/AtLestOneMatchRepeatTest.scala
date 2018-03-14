package com.wang.regex

import org.scalatest.{FunSpec, Matchers}

class AtLestOneMatchRepeatTest extends FunSpec with Matchers{
  val atLeastOnMatch = AtLestOneMatchRepeat(Literal('c'))
  describe("input contains"){
    it("shoud output many streams"){
      atLeastOnMatch.regexMatch(List("ccca","da")) should contain theSameElementsAs List("ccca","da","cca","ca","a")
    }
  }
}
