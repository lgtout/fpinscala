package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.

  def main(args: Array[String]): Unit = {
    var l: List[Int] = null
    var t: List[Int] = null
    var e: List[Int] = null
    var l1: List[Int] = null
    var r: List[Int] = null
    var n: Int = 0
    var h: Int = 0
    var f: Int => Boolean = null

//    l = List(1,2,3)
//    t = tail(l)
//    e = List(2,3)
//    println("l: %s, t: %s, e: %s".format(l, t, e))
//    assert(t == e)
//
//    l = List()
//    t = Nil
//    e = Nil
//    println("l: %s, t: %s, e: %s".format(l, t, e))
//    assert(t == e)
//
//    l = List(1,2,3)
//    h = 0
//    e = List(0,1,2,3)
//    l1 = setHead(l, h)
//    println("l: %s, h: %s, l1: %s, e: %s".format(l, h, l1, e))
//    assert(l1 == e)
//
//    l = Nil
//    h = 1
//    e = List(1)
//    l1 = setHead(l, h)
//    println("l: %s, h: %s, l1: %s, e: %s".format(l, h, l1, e))
//    assert(l1 == e)
//
//    l = List(1,2,3)
//    n = 0
//    e = List(1,2,3)
//    l1 = drop(l, n)
//    println("l: %s, n: %s, l1: %s, e: %s".format(l, n, l1, e))
//    assert(l1 == e)

//    l = List(1,2,3)
//    n = 1
//    e = List(2,3)
//    l1 = drop(l, n)
//    println("l: %s, n: %s, l1: %s, e: %s".format(l, n, l1, e))
//    assert(l1 == e)

//    l = List(1,2,3)
//    f = a => a == 2
//    r = dropWhile(l, f)
//    e = List(1,3)
//    println("l: %s, f: %s, r: %s, e: %s".format(l, f, r, e))
//    assert(r == e)
//
//    l = List(1,2,3)
//    f = a => a == 3
//    r = dropWhile(l, f)
//    e = List(1,2)
//    println("l: %s, f: %s, r: %s, e: %s".format(l, f, r, e))
//    assert(r == e)
//
//    l = List(1,2,3)
//    f = a => a == 4
//    r = dropWhile(l, f)
//    e = List(1,2,3)
//    println("l: %s, f: %s, r: %s, e: %s".format(l, f, r, e))
//    assert(r == e)
//
//    l = List()
//    f = a => a == 4
//    r = dropWhile(l, f)
//    e = List()
//    println("l: %s, f: %s, r: %s, e: %s".format(l, f, r, e))
//    assert(r == e)
//
//    l = Nil
//    f = a => a == 4
//    r = dropWhile(l, f)
//    e = Nil
//    println("l: %s, f: %s, r: %s, e: %s".format(l, f, r, e))
//    assert(r == e)

//    l = List(1,2,3)
//    r = init(l)
//    e = List(1,2)
//    println("l: %s, r: %s, e: %s".format(l, r, e))
//    assert(r == e)
//
//    l = List()
//    r = init(l)
//    e = List()
//    println("l: %s, r: %s, e: %s".format(l, r, e))
//    assert(r == e)
//
//    l = List(1)
//    r = init(l)
//    e = Nil
//    println("l: %s, r: %s, e: %s".format(l, r, e))
//    assert(r == e)

//    l = List(1,2,3)
//    f = a => a == 2
//    r = dropWhile_withImprovedTypeInference(l)(f)
//    e = List(1,3)
//    println("l: %s, f: %s, r: %s, e: %s".format(l, f, r, e))
//    assert(r == e)
//
//    l = List(1,2,3)
//    f = a => a == 3
//    r = dropWhile_withImprovedTypeInference(l)(f)
//    e = List(1,2)
//    println("l: %s, f: %s, r: %s, e: %s".format(l, f, r, e))
//    assert(r == e)
//
//    l = List(1,2,3)
//    f = a => a == 4
//    r = dropWhile_withImprovedTypeInference(l)(f)
//    e = List(1,2,3)
//    println("l: %s, f: %s, r: %s, e: %s".format(l, f, r, e))
//    assert(r == e)
//
//    l = List()
//    f = a => a == 4
//    r = dropWhile_withImprovedTypeInference(l)(f)
//    e = List()
//    println("l: %s, f: %s, r: %s, e: %s".format(l, f, r, e))
//    assert(r == e)
//
//    l = Nil
//    f = a => a == 4
//    r = dropWhile_withImprovedTypeInference(l)(f)
//    e = Nil
//    println("l: %s, f: %s, r: %s, e: %s".format(l, f, r, e))
//    assert(r == e)
    
  }

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // 3
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case _ => Cons(h, l)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    println("%s, %s".format(l, n))
    (l, n) match {
      case (Nil, _) => Nil
      case (_, 0) => l
      case (Cons(_, xs), _) =>
        drop(xs, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(a, xs) => if (f(a)) dropWhile(xs, f) else Cons(a, dropWhile(xs, f))
  }

  def dropWhile_withImprovedTypeInference[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(a, xs) => if (f(a)) dropWhile(xs, f) else Cons(a, dropWhile(xs, f))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(a, Cons(_, Nil)) => Cons(a, Nil)
    case Cons(a, xs) => Cons(a, init(xs))
  }

  def length[A](l: List[A]): Int = ???

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}
