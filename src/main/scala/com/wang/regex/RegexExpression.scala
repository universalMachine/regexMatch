package com.wang.regex



import typedef.{Pos, State}
import helper.logger

import scala.util.control.{Breaks, ControlThrowable}

trait RegexExpression {
  def regexMatch(remainStreams: State[String]): State[String]
}

case class Literal(value :Char) extends RegexExpression{
  override def regexMatch(remainStreams: State[String]) = remainStreams match {
    case Nil => Nil
    case remains => remains.map(remain =>{
      if(remain.charAt(0) == value){
         remain.substring(1)
      }
      else
        null
    }).filterNot(str => str==null);
  }
}

case class Alternative(left:RegexExpression,right: RegexExpression) extends RegexExpression{
  override def regexMatch(remainStreams: State[String]): State[String] = remainStreams match{
    case Nil => Nil
    case remainStreams =>{
      left.regexMatch(remainStreams):::(right.regexMatch(remainStreams))
    }
  }
}
case class Repeat(Repeation: RegexExpression) extends RegexExpression{
  override def regexMatch(remainStreams: State[String]): State[String] = remainStreams match{
    case Nil => Nil
    case remainStreams =>{
      remainStreams ::: this.regexMatch(Repeation.regexMatch(remainStreams))
   }
  }
}


case class AtLestOneMatchRepeat(Repeation: RegexExpression) extends RegexExpression{
  override def regexMatch(remainStreams: State[String]): State[String] = remainStreams match{
    case Nil => Nil
    case remainStreams =>{
      Repeat(Repeation).regexMatch(Repeation.regexMatch(remainStreams))
    }
  }
}

case class Sequence(prev:RegexExpression,next:RegexExpression) extends RegexExpression{
  override def regexMatch(remainStreams: State[String]): State[String] = remainStreams match{
    case Nil => Nil
    case remainStreams =>{
      next.regexMatch(prev.regexMatch(remainStreams))
    }
  }
}

/* var procssStrems = remainStreams
   var allStreams: State[String] = remainStreams
   try{
     while(true){
       procssStrems = Repeation.regexMatch(procssStrems)
       if(procssStrems == Nil){
         Breaks.break()
       }
       allStreams = allStreams ::: procssStrems
   }}catch{
     case c: ControlThrowable => logger.debug(c.getMessage)
     case t: Throwable => logger.debug(t.getMessage)
   }

   allStreams*/

