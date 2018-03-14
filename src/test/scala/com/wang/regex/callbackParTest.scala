package com.wang.regex


import java.util.concurrent.{ExecutorService, Executors}

import callbackPar._
import org.scalatest.FunSuite

class callbackParTest extends FunSuite {

 // val p = Nonblocking.Par.parMap(List.range(1,6))(math.sqrt(_))
  val hugeList = List.range(0,30);
  val p2 = ParMap(hugeList)(math.sqrt(_))
  //val p3 = parFoldBalance(hugeList.toIndexedSeq)(a=>a)((b1,b2)=>b1+b2)
  val es = Executors.newFixedThreadPool(2)
  val g = Gen.unit(map2(unit(235),unit(6))(_+_))
  val g2 = Gen.unit(hugeList)
  val repeatList = List.fill(10)("1").zipWithIndex ++ List.fill(10)("2").zipWithIndex
  test("testParMap") {
   //val p2 = lazyUnit(30*69)
    //println(callbackPar.run(es)(p3))
  }

  test("testParGroupBy"){
  /* IO.PrintLine(parGroupBy(repeatList)((a1,a2)=> (a1._1+a2._1,a1._2+a2._2))
       .run(es).toString()).run*/
  }

  test("testJoin"){
    PropTest.run(Gen.forAll(g)(par=> join(unit(par)).run(es) !=null),1)
  }

  test("testRun"){
   PropTest.run(Gen.forAll(g)(par=>unit(par).map(p2=>callbackPar.run(es)(p2)).run(es) == 241),1)

  }
  def produceUnit:Unit = {return  5}

  test("unit"){
    println(unit(5)(es)(a=>a) == ())
    println(produceUnit == ())
  }

  test("testflatMap"){
    PropTest.run(Gen.forAll(g)(par=> flatMap(par)(result=>unit(result)).run(es)+20 == 2),300)
  }

  test("testFoldBanlance"){
    parFoldBalance(hugeList.toIndexedSeq)(a=>a)(_+_).run(es)
  }



}
