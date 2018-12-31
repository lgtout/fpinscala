package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // 3.25
  def size[A](t: Tree[A]): Int = {
    t match  {
      case Leaf(_) => 1
      case Branch(a, b) => size(a) + size(b)
    }
  }

  // 3.26
  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(a) => a
      case Branch(a, b) => maximum(a) max maximum(b)
    }
  }

  // 3.27
  def maxPathLength[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 0
      case Branch(a, b) => (maxPathLength(a) max
        maxPathLength(b)) + 1
    }
  }

  // 3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(a, b) => Branch(map(a)(f), map(b)(f))
    }
  }

  // 3.29
//  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
//    case Leaf(a) => f(a)
//    case Branch(l, r) => g(fold(l), fold(r))(f)(g)
//  }

//  def sizeViaFold[A](t: Tree[A]): Int = {
//    fold(t)(_ => 1)(_ + _ + 1)
//  }
//
//  def maximumViaFold(t: Tree[Int]): Int = {
//    fold(t)(a => a)(_ max _)
//  }

//  def depthViaFold[A](t: Tree[A]): Int = {
//    fold(t)(_ => 0)(_ max _ + 1)
//  }
//
//  def mapViaFold[A,B](t: Tree[A])(f: A => B) : Tree[B] = {
//    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
//  }

  def main(args: Array[String]): Unit = {

    // 3.25
//    var t: Tree[Char] = Leaf('a')
//    assert(size(t) == 1)
//    t = Branch(Leaf('a'), Leaf('b'))
//    assert(size(t) == 2)
//    t = Branch(Leaf('a'), Branch(Leaf('b'), Leaf('c')))
//    assert(size(t) == 3)

    // 3.26
//    var t: Tree[Int] = Leaf(1)
//    assert(maximum(t) == 1)
//    t = Branch(Leaf(1), Leaf(2))
//    assert(maximum(t) == 2)
//    t = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
//    assert(size(t) == 3)

    // 3.27
//    var t: Tree[Int] = Leaf(1)
//    assert(maxPathLength(t) == 0)
//    t = Branch(Leaf(1), Leaf(2))
//    assert(maxPathLength(t) == 1)
//    t = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
//    assert(maxPathLength(t) == 2)
//    t = Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
//    assert(maxPathLength(t) == 3)

    // 3.28
//    var t: Tree[Int] = Leaf(1)
//    assert(maxPathLength(t) == 0)
//    t = Branch(Leaf(1), Leaf(2))
//    assert(maxPathLength(t) == 1)
//    t = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
//    assert(maxPathLength(t) == 2)
//    t = Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
//    assert(maxPathLength(t) == 3)
  }

}

