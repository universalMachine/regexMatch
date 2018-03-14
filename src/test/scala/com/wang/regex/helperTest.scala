package com.wang.regex
import helper2._
import org.scalatest.{FunSuite, Matchers}

class helperTest extends FunSuite with Matchers{

  test("testFindFirstNotMatchIndex") {
    findFirstNotMatchIndex("","") should be(-1)
    findFirstNotMatchIndex("woa","woa34") should be(-1)
    findFirstNotMatchIndex("woa","wo") should be(2)
  }

}
