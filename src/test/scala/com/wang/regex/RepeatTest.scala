package com.wang.regex
import org.scalatest.{FunSpec, Matchers}

import scala.collection.immutable

class RepeatTest extends FunSpec with Matchers{
  val repeatc = Repeat(Literal('c'))
  describe("input contains"){
    it("shoud output many streams"){
      repeatc.regexMatch(List("ccca","da")) should contain theSameElementsAs List("ccca","da","cca","ca","a")
    }
  }


}
