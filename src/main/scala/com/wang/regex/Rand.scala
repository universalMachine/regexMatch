package com.wang.regex


/*trait RNG[A]{
  def next: (A,RNG[A])
}*/

trait RNG{
  def next: (Int,RNG)
  def genFixedRand(n: Int):(List[Int],RNG)={
    def go(list:List[Int],rng:RNG,n:Int):(List[Int],RNG) = {
      if(n != 0 ){
        val (result,newRng) = this.next
        go(result::list,newRng,n-1)
      }
      else
        (list,rng)
    }
    go(List.empty[Int],this,n)
  }
}


object State{
  type State[A,S]= S=>(A,S)

  def unit[A,S](a: A):State[A,S] = {state:S=> (a,state)}
  def map[A,S,B](s :State[A,S])(f: A=>B):State[B,S] = map2(s,unit[Unit,S](())){case (a,_) => f(a)}

  def map2[A,S,B,C](s1:State[A,S],s2:State[B,S])(f: (A,B) =>C): State[C,S] = { state:S =>{
    (f(s1(state)._1,s2(state)._1),state)
  }}



}

case class SimpleRNG(seed: Long) extends RNG {
  def next = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
    val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
    val n  = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
    (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
  }
}

/*
object Rand {

  import State._
  def randomPair[A](rng: RNG[A]):((A,A),RNG[A]) ={
    val (i1,rng1) = rng.next
    val (i2,rng2) = rng1.next
    ((i1,i2),rng2)
  }

  def nonNegativeInt: State[Int,RNG[Int]] = { rng:RNG[Int] => {
    val (i,rng1) = rng.next
    (if(i<0) -(i+1) else i,rng1)
  }}

  def double[A]: State[Double,RNG[Int]] = map(nonNegativeInt)(_.toDouble/Int.MaxValue)

  def intDouble: State[(Int,Double),RNG[Int]] = map2(nonNegativeInt,double)((_,_))
}

*/
