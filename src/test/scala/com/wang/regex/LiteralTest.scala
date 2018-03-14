package com.wang.regex

import org.scalatest.FunSpec
import helper.logger;

class LiteralTest extends FunSpec {
  val literal = Literal('c')
  describe("Literal"){
    describe("when input is Nil"){
      it("shoud output Nil"){
        assert(literal.regexMatch(Nil) == Nil)
      }
    }

    describe("when input is not equal"){
      it("should output Nil"){
        assert(literal.regexMatch(List("3")) == Nil)
      }
    }

    describe("when input is equal"){
      describe("when has one input stream"){
        it ("should delete self from every stream"){
          assert(literal.regexMatch(List("c","cg")) == List("","g"))
        }
      }
    }
  }

}
