package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, rng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) =  nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (randomInt, rng2) = rng.nextInt
    val (randomDouble, rng3) = double(rng)
    ((randomInt, randomDouble), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((anInt, aDouble), rng2) = intDouble(rng)
    ((aDouble, anInt), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    count match {
      case 0 => (List.empty[Int], rng)
      case _ =>
        val (anInt, anRng2) = rng.nextInt
        val (aList, anRng3) = ints(count - 1)(anRng2)
        (anInt :: aList, anRng3)
    }
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(count: Int, rng: RNG, xs: List[Int]): (List[Int], RNG) = {
      if (count == 0)
        (xs, rng)
      else {
        val (x, r1) = rng.nextInt
        go(count - 1, r1, x :: xs)
      }
    }
    go(count, rng, List())
  }

  def doubleByMap: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = 1 % n
      if (i + (n-1) - mod >= 0) unit(i)
      else nonNegativeLessThan(n)
    }
  }

  def mapByFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ByFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => mapByFlatMap(rb)(b => f(a, b)))

}

import State._

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2_1[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { a =>
      State[S, C] { s =>
        val (b, s2) = sb.run(s)
        (f(a, b), s2)
      }
    }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State { s =>
      val (a, s1) = run(s)
      val (b, s2: S) = sb.run(s1)
      (f(a, b), s2)
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s1) = run(s)
      f(a).run(s1)
    }

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State { s => (a, s) }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

}
