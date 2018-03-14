package com.wang.regex

import org.scalatest.FunSuite

class IOTest extends FunSuite {
  test("testFlatmap") {
    //IO.ReadLine.flatMap(IO.PrintLine)
    IO.fileStream(".").run.toList
  }
}
