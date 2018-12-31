package fpinscala.datastructures

import scala.collection.mutable

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
    var ei: Int = 0
    var l1: List[Int] = null
    var r: List[Int] = null
    var ri: Int = 0
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

//    l = Nil
//    ri = length(l)
//    ei = 0
//    println("l: %s, ri: %s, e: %s".format(l, ri, e))
//    assert(ri == ei)
//
//    l = List(1)
//    ri = length(l)
//    ei = 1
//    println("l: %s, ri: %s, e: %s".format(l, ri, e))
//    assert(ri == ei)
//
//    l = List(1,2,3,4)
//    ri = length(l)
//    ei = 4
//    println("l: %s, ri: %s, e: %s".format(l, ri, e))
//    assert(ri == ei)

//    l = List(2,4,16,256)
//    ri = foldLeft(l, 1)((x,y) => y / x) // 256/(16/(4/(2/1)))
//    ei = 32
//    println("l: %s, ri: %s, ei: %s".format(l, ri, ei))
//    assert(ri == ei)

//    l = List(1,2,3,4)
//    r = reverse1(l)
//    e = List(4,3,2,1)
//    println("l: %s, r: %s, e: %s".format(l, r, e))
//    assert(r == e)

//    l = List(1,2)
//    l1 = List(3,4)
//    r = append2(l, l1)
//    e = List(1,2,3,4)
//    println("l: %s, l1: %s, r: %s, e: %s".format(l, l1, r, e))
//    assert(r == e)

//    val l2 = filter(List(1,2,3,2))(_ == 2)
//    assert(l2 == List(1,3))
//    println(l2)
//
//    val l3 = filter(
//      List('a','b','c','a'))(_ == 'a')
//    assert(l3 == List('b','c'))
//    println(l3)
//
//    val l4 = filter(List(2))(_ == 2)
//    assert(l4 == List())
//    assert(l4 == Nil)
//    println(l4)

//    val l2 = map(List(1,2,3,1,2))(a => if (a == 1) "a" else if (a == 2) "b" else "")
//    println(l2)
//    assert(l2 == List("a","b","","a","b"))

//    val l2 = flatMap(List(1,2,3))(a => List(a + 1, a + 2, a + 3))
//    println(l2)
//    assert(l2 == List(2,3,4,3,4,5,4,5,6))

    import scala.collection.immutable.{List => SList}
    val l2 = filter_byFlatMap(List(1,2,3,2))(
      SList(1,3).contains(_))
    println(l2)
    assert(l2 == List(1,3))

    val l3 = filter_byFlatMap(List('a','b','c','a'))(_ != 'a')
    println(l3)
    assert(l3 == List('b','c'))

    val l4 = filter_byFlatMap(List(2))(_ != 2)
    println(l4)
    assert(l4 == List())
    assert(l4 == Nil)
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
    case Nil => sys.error("setHead on empty list")
    case Cons(_, xs) => Cons(h, xs)
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

//  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
//    case _ => l
//    case Cons(a, xs) if f(a) => dropWhile(xs, f)
//  }

//  def dropWhile_withImprovedTypeInference[A](l: List[A])(f: A => Boolean): List[A] = l match {
//    case Nil => Nil
//    case Cons(a, xs) => if (f(a)) dropWhile(xs, f) else Cons(a, dropWhile(xs, f))
//  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(a, xs) => Cons(a, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, y) => 1 + y)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(a, as) => foldLeft(as, f(z, a))(f)
    }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)((x,y) => x + y)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((n, _) => 1 + n)

  // Specify type instead of using asInstanceOf[]
  def reverse2[A](l: List[A]): List[A] = foldLeft[A, List[A]](l, Nil)((b, a) => Cons(a, b))

  // Use asInstanceOf[] instead of specifying type.
  def reverse1[A](l: List[A]): List[A] = foldLeft(l, Nil.asInstanceOf[List[A]])((b, a) => Cons(a, b))

  def append2[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((h,t) => Cons(t, h))

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B) : B =
    foldLeft(reverse(l), z)((h, t) => f(t, h))

  // 3.16
  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  // 3.17
  def toString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  // 3.18
  def map_1[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  // TODO Repeat implementing from memory
  // map using foldRightViaFoldLeft
  def map_2[A,B](l: List[A])(f: A => B): List[B] = ???

  // TODO Repeat implementing from memory
  // map using buffer
  def map_3[A,B](l: List[A])(f: A => B): List[B] = ???

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  // 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((a, b) => append(f(a), b))

  // 3.21
  def filter_byFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil: List[A])

  // 3.22
  // Redo from memory
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b)
    match {
      case(_, Nil) => Nil
      case(Nil, _) => Nil
      case(Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h1, addPairwise(t1, t2))
  }

  // 3.23
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b)
  match {
    case(_, Nil) => Nil
    case(Nil, _) => Nil
    case(Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  // 3.24
  // Do


}
