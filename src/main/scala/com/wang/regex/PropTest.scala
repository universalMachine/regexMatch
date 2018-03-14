package com.wang.regex

import com.wang.regex.State.State
import typedef1._
import helper.logger
import scala.collection.immutable.Stream.Empty
 object typedef1{
  type SuccessCount = Int
  type FailedCase = String
  type TestCaseNum = Int
   type MaxSize = Int
}
trait Result{
  def isFailed: Boolean
  def advancePassed(n:Int):Result = this match {
    case Passed(m) => Passed(n+m)
    case Failed(failcase,m) => Failed(failcase,n+m)
  }
}


  case class  Passed(success:SuccessCount) extends Result{
    override def isFailed: Boolean = false
  }

  case class Failed(failure: FailedCase,success: SuccessCount) extends Result{
    override def isFailed: Boolean = true
  }




case class Prop(run: (MaxSize,TestCaseNum,RNG) => Result ){
  def &&(prop:Prop): Prop =Prop{
    (max,n,rng)=> this.run(max,n,rng) match {
      case Passed(n) => prop.run(max,n,rng).advancePassed(n)
      case fail@Failed(_,_)=>fail
    }
  }
}


case class Gen[A](sample:State[A,RNG])

object Gen{



  def unit[A](a: A):Gen[A] = Gen[A](rng=>(a,rng))
  def boolean(a:Boolean):Gen[Boolean] = unit(a)

  //def listofN[A](n:Int,g:Gen[A]):Gen[List[A]] =Gen[List[A]]{rng => List(n).map(_=>g.sample(rng)).unzip()}
  def forAll[A](g:Gen[A])(f: A=>Boolean):Prop = Prop{
    (max,n,rng)=> buildRandomStream(rng)(g).take(n).zip(Stream.from(0)).map{
      case (testCase,i)=> try{
        logger.debug(s"$testCase")
        if(f(testCase)){ Passed(i)} else{Failed(testCase.toString(),i)}
      }catch{ case e:Exception => Failed(buildMsg(testCase,e),i)}
    }.find(_.isFailed).getOrElse(Passed(n))
  }
  def buildRandomStream[A,S](rng:RNG)(g: Gen[A]):Stream[A] = myStream.unfold(rng)(s=>Some(g.sample(rng)))
  def buildMsg[A](testCase: A,e:Exception):String =  s"test case: $testCase\n"+
    s"stack trace:\n ${e.getStackTrace.mkString("\n")})"
  def forAll[A](g: Int=>Gen[A])(f: A=>Boolean):Prop = Prop{
    (max,n,rng)=>{
      val perSize = (n+(max-1))/max;
      /**
        * 为每一个长度构建一个测试
        */
      val props:Stream[Prop]= Stream.from(0).take(n.min(max)).map(i=>forAll(g(i))(f))
        props.map(p=>Prop{
          /**
            *平均分配每个长度的测试
            */
          (max,_,rng) => p.run(max,perSize,rng)
      }).reduce(_&&_).run(max,n,rng)
    }
  }
}



object myStream{


  def unfold[A,S](z: S)(f:S=>Option[(A,S)]): Stream[A] = f(z) match{
    case None =>  Empty
    case Some((a,s)) => Stream.cons(a,unfold(s)(f))
  }
}

case object PropTest{

  def run[A](prop: Prop,testCaseNum: TestCaseNum= 100,max:MaxSize=100,rng:RNG= SimpleRNG(System.currentTimeMillis())):Unit =
    prop.run(max,testCaseNum,rng) match{
      case Failed(msg,n) => println(s"! Failed at $msg;passed tests: $n")
      case Passed(n) => println("passed")
    }

}