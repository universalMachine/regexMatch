package com.wang.regex

import org.scalatest.{FunSuite, Matchers}

class RandTest extends FunSuite with Matchers{
  val simpleRNG = SimpleRNG(34)
  val simpleRNG2 = simpleRNG.next._2
  test("testIntDouble") {
    /*Rand.intDouble(simpleRNG) should equal(Rand.intDouble(simpleRNG))
    Rand.intDouble(simpleRNG) should not equal(Rand.intDouble(simpleRNG2))
    Rand.intDouble(simpleRNG2) should  equal(Rand.intDouble(simpleRNG2))*/
  }
}
