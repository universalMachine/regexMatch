package com.wang.regex

import org.scalatest.FunSuite
import Gen._
class PropTestTest extends FunSuite {
  val g = unit(323)
  val FixedLenGen =(n:Int)=> Gen(rng=>(rng.genFixedRand(n),SimpleRNG(200)))
  test("forall"){
    PropTest.run(forAll(g)(i=>i>20))
  }
  test("forallDynamicGen"){
    PropTest.run(forAll(FixedLenGen)(i=>i.length<8),1)
  }
}
