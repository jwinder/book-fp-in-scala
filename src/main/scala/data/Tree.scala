package data

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def leaf[A](a: A): Tree[A] = Leaf(a)
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def branch[A](pair: Tuple2[Tree[A], Tree[A]]): Tree[A] = branch(pair._1, pair._2)

  // exercise 3.25
  // counts all leafs, branches
  def size2[A](as: Tree[A]): Int = as match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size2(left) + size2(right)
  }

  // exercise 3.26
  def maximum2(ns: Tree[Int]): Int = ns match {
    case Leaf(n) => n
    case Branch(left, right) => math.max(maximum2(left), maximum2(right))
  }

  // exercise 3.27
  def depth2[A](as: Tree[A]): Int = as match {
    case Leaf(n) => 1
    case Branch(left, right) => 1 + math.max(depth2(left), depth2(right))
  }

  // exercise 3.28
  def map2[A,B](as: Tree[A])(f: A => B): Tree[B] = as match {
    case Leaf(a) => leaf(f(a))
    case Branch(left, right) => branch(map2(left)(f), map2(right)(f))
  }

  // exercise 3.29
  def fold[A,B](as: Tree[A])(f: A => B)(g: (B,B) => B): B = as match {
    case Leaf(a) => f(a)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def size[A](as: Tree[A]): Int = fold(as)(_ => 1)(_ + _ + 1)
  def maximum(ns: Tree[Int]): Int = fold(ns)(identity)(math.max)
  def depth[A](as: Tree[A]): Int = fold(as)(_ => 1)(math.max(_,_) + 1)
  def map[A,B](as: Tree[A])(f: A => B): Tree[B] = fold(as)(a => leaf(f(a)))(branch)
}
