package fpinscala.testing

import fpinscala.state._
import fpinscala.testing.Prop.{FailedCase, SuccessCount}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop_ {
  def check: Boolean
  def &&(prop: Prop_): Prop_ = new Prop_ {
    override def check: Boolean = this.check && prop.check
  }
}

object Prop_ {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop_ = ???
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
}


object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
}

sealed trait Result {
  def isFalsified: Boolean
  def &&(r: Result): Result =
    if (isFalsified) this
    else r
  def ||(r: Result): Result =
    if (!isFalsified) this
    else r
}
case object Passed extends Result {
  override def isFalsified: Boolean = false
}
case class Falsified(failure: FailedCase,
                    successes: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}

import Prop._
case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop =
    Prop{(n, rng) => run(n, rng) && p.run(n, rng)}
  def ||(p: Prop): Prop =
    Prop{(n, rng) => run(n, rng) || p.run(n, rng)}
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(_ + start))
  def unit[A](a: => A): Gen[A] =
    Gen(State(s => (a, s)))
  def boolean: Gen[Boolean] =
    Gen(State(RNG.int).map(_ < 0))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
//    if (n == 0) unit(Nil)
//    else Gen(g.sample.map2(listOfN(n - 1, g).sample)(_ :: _))
    Gen(State.sequenceViaFoldLeft(List.fill(n)(g.sample)))
  def sequence[A](list: List[Gen[A]]): Gen[List[A]] =
    Gen(State.sequenceViaFoldLeft(list.map(_.sample)))
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map[B](f))
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))
  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen[C](sample.map2(g.sample)(f))
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(Gen.listOfN(_, this))
  def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def unit[A](a: => A): SGen[A] = SGen(_ => Gen.unit(a))
  def map[B](f: A => B): SGen[B] =
    SGen(n => forSize(n).map(f))
  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => forSize(n).flatMap {a => f(a).forSize(n)})
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN(n, g))
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN(if (n >= 1) n else 1, g))
}
