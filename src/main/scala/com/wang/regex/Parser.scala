package com.wang.regex


import errorHandle._
import Parser._
import helper.logger



object helper2{
  def findFirstNotMatchIndex(s1: String,s2: String):Int = {
    var i: Int = 0
    while( i<s1.length && i< s2.length && s1.charAt(i) == s2.charAt(i)){
      i += 1
    }
    if(i == s1.length)
      return -1
    else
      return i
  }

}


trait ParseResult[+A]{
  def mapError(f: ParseError => ParseError):ParseResult[A] = this match{
    case ParseFail(parseError,isCommit: Boolean) => ParseFail(f(parseError),isCommit)
    case _ => this
  }

  def unCommit: ParseResult[A] = this match {
    case ParseFail(e,true) => ParseFail(e,false)
    case _ => this
  }

  def addCommit(isShouldCommit: Boolean): ParseResult[A] = this match {
    case ParseFail(e,isCommit) => ParseFail(e,isCommit||isShouldCommit)
    case _ => this
  }

  def advanceSuccess(n: Int): ParseResult[A] = this match{
    case ParseSuccess(result,m) => ParseSuccess(result,n+m)
    case _ => this
  }


}
case class ParseSuccess[A](get: A,cosumedChar: Int) extends ParseResult[A]
case class ParseFail[A](get: ParseError,isCommit: Boolean) extends ParseResult[Nothing]


object Parser{
  type Parser[A] =  ParserState => ParseResult[A]
  case class ParserState(loc: Location){
    def input: String = loc.input.substring(loc.offset)
  }


}

object errorHandle{
  case class Location(input: String,offset: Int){

    lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset+1).lastIndexOf('\n') match {
      case  -1 => offset + 1
      case lineStart => offset - lineStart
    }

    def advanceBy(n: Int):Location ={
      copy(offset = offset + n)
    }

  }


  case class ParseError(stack: List[(Location,String)]){
    def push(loc:Location,msg: String):ParseError = copy(stack = (loc,msg) :: stack)
    def label(loc:Location,msg: String):ParseError = ParseError(latestLoc.map((_,msg)).toList)
    def latestLoc: Option[Location] = stackLatest.map(_._1)
    def stackLatest : Option[(Location,String)] = stack.lastOption


    /**
    Display collapsed error stack - any adjacent stack elements with the
  same location are combined on one line. For the bottommost error, we
  display the full line, with a caret pointing to the column of the error.
  Example:
  1.1 file 'companies.json'; array
  5.1 object
  5.2 key-value
  5.10 ':'
  { "MSFT" ; 24,
      */
    override def toString =
      if (stack.isEmpty) "no error message"
      else {
        val collapsed = collapseStack(stack)
        val context =
          collapsed.lastOption.map(_._1).getOrElse("") +
            collapsed.lastOption.map( _._1).getOrElse("")

          collapsed.map { case (locStr,msg) => locStr+": "+msg}.mkString("\n")
      }

    /* Builds a collapsed version of the given error stack -
     * messages at the same location have their messages merged,
     * separated by semicolons */
    def collapseStack(s: List[(Location,String)]): List[(String,String)] =
      s.map{case  (loc:Location,str)=> ("line: "+loc.line+",col: "+loc.col,str)}.groupBy(_._1).
        mapValues(_.map(_._2).mkString("; ")).
        toList

    def formatLoc(l: Location): String = l.line + "." + l.col
  }
}



trait  Parsers{self =>

  //凡是Parser[A]都可以使用ParserOps
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  //range

  def attempt[A](p1: Parser[A]):Parser[A] = state => p1(state).unCommit


  def scope[A](msg: String,p: Parser[A]): Parser[A]

  def label[A](msg:String)(p: Parser[A]): Parser[A]

  def map[A,B] (a: Parser[A])(f: A=>B):Parser[B] = flatMap(a)(f.andThen(succed[B]))

  def flatMap[A,B](p1: Parser[A])(f: A=>Parser[B]): Parser[B] = state => p1(state) match {
    case fail @ ParseFail(e,isCommit) => fail
    case ParseSuccess(get,n) => logger.debug(state.loc.offset+";"+n.toString);f(get)(state.copy(loc=state.loc.advanceBy(n))).addCommit(n!=0).advanceSuccess(n)
  }

  def succed[A](a:A):Parser[A] = state => ParseSuccess(a,0)


  def char(c:Char):Parser[Char]

  def run[A](p: Parser[A])(input: String) : ParseResult[A]

  def product[A,B](prev: Parser[A],next: Parser[B]):Parser[(A,B)] = flatMap(prev)( prevResult => map(next)(nextResult=>(prevResult,nextResult)))

  def or[A](p1: Parser[A],p2: Parser[A]): Parser[A]

  def awalyasFail[A](a: A):Parser[A] = state=>{ ParseFail( ParseError(List((state.loc,"always fail"))), false)}

  def many[A](p: Parser[A]):Parser[List[A]] = attempt(p.flatMap(result => many(p).map(results=>result :: results))) | succed(Nil)


  def many1[A](p: Parser[A]):Parser[List[A]] = (p & many(p)).map {case (a,lista)=> a :: lista}

  implicit def string(s: String): Parser[String]

  /**
    * 这些都是为了实现中缀表达式
    */
  //能转化成string的也可以用
  implicit def asStringToParser[A](a: A)(implicit f: A=>Parser[String]): ParserOps[String] = ParserOps(f(a))
  //包裹类
  case class ParserOps[A](p1: Parser[A]){
    def | [B>:A](p2:Parser[B]) = self.or(p1,p2)
    def map[B](f: A=>B) = self.map(p1)(f)
    def &[B](next: Parser[B]) = product(p1,next)
    def ** = many(p1)

    def flatMap[B](f: A=>Parser[B]): Parser[B] = self.flatMap(p1)(f)


  }


}

object RegexParsers extends Parsers{


  override def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  override def run[A](p: Parser[A])(input: String): ParseResult[A] = {
    p(ParserState(Location(input,0)))
  }


  override def or[A](p1: Parser[A],p2: Parser[A]): Parser[A] = state =>{
    p1(state) match{
      case ParseFail(e,false) => p2(state) match {
        case fail @ ParseFail(error,_) => fail.copy(get = error.copy(stack = e.stack ::: error.stack ))
        case x @_ =>  x
    }
      case otherResult => otherResult
    }
  }

  override implicit def string(s: String): Parser[String] = state => {
    val offset = state.loc.offset;
    if(offset == state.loc.input.length){
      ParseFail( ParseError((state.loc,"已到末尾")::Nil),true)
    }else
    {
    helper2.findFirstNotMatchIndex(s,state.input) match {
      case -1 => logger.trace(" string: "+state.input+";");ParseSuccess(s, s.length())
      case notMatchIndex => {
        val advancedLoc = state.loc.advanceBy(notMatchIndex)
        val ExpectedChar  = s.charAt(notMatchIndex)
        val ActualChar = state.input.charAt(notMatchIndex)
        val common = s.substring(0,notMatchIndex).map(c => if(c =='\n') "\\n" else c).mkString
        val msg = "Expected: "+common+"["+ExpectedChar+"]"+" Actual: "+common+"["+ ActualChar +"]"
        ParseFail(ParseError((advancedLoc,msg)::Nil)
          ,true)
      }
    }
  }

  }

  override def scope[A](msg: String,p: Parser[A]): Parser[A] =  (state) =>{
    logger.debug("state:"+state)
    p(state).mapError(_.push(state.loc,msg))
  }


  override def label[A](msg: String)(p: Parser[A]): Parser[A] = (state)=>{
    p(state).mapError(_.label(state.loc,msg))
  }

  def main(args: Array[String]): Unit = {

  }


}






