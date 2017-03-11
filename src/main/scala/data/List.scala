package example.data
import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def empty[A]: List[A] = Nil

  // exercise 3.1
  // answer 1

  // exercise 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  // exercise 3.3
  def setHead[A](head: A, as: List[A]): List[A] = {
    Cons(head, tail(as))
  }

  // exercise 3.4
  @tailrec
  def drop[A](as: List[A], n: Int): List[A] = n match {
    case done if n <= 0 => as
    case continue => drop(tail(as), n - 1)
  }

  // exercise 3.5
  @tailrec
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case finishedList => finishedList
  }

  def append2[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(head, tail) => Cons(head, append(tail, a2))
  }

  def appendElement2[A](as: List[A], a: A): List[A] = append2(as, List(a))

  // exercise 3.6
  def init2[A](as: List[A]): List[A] = {
    @tailrec
    def init0[A](rebuilt: List[A], leftover: List[A]): List[A] = leftover match {
      case Nil => rebuilt
      case Cons(head, Nil) => rebuilt
      case Cons(head, tail) => init0(appendElement2(rebuilt, head), tail)
    }

    init0(Nil, as)
  }

  def foldRight2[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  // exercise 3.7
  // product2 doesn't have control of the recursive call currently
  // foldRight would have to be implemented differently

  // exercise 3.8
  // foldRight can be used to construct a list

  // exercise 3.9
  def length2[A](as: List[A]): Int = foldRight[A,Int](as, 0)((_, acc) => acc + 1)

  // exercise 3.10
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = {
    @tailrec
    def foldLeft0(as: List[A], acc: B): B = as match {
      case Nil => acc
      case Cons(head, tail) => foldLeft0(tail, f(acc, head))
    }
    foldLeft0(as, z)
  }

  // exercise 3.11
  def sum(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def product(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
  def length[A](as: List[A]) = foldLeft(as, 0)((acc, _) => acc + 1)

  // exercise 3.12
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List.empty[A])((acc, a) => Cons(a, acc))

  // exercise 3.13
  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = {
    val foldRight0: B => B = foldLeft(as, identity[B](_)) { (continuation, a) =>
      b => continuation(f(a,b))
    }
    foldRight0(z)
  }

  // exercise 3.14
  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((a, acc) => Cons(a, acc))

  // exercise 3.15
  def appendAll[A](lists: List[List[A]]) =
    foldLeft(lists, List.empty[A])((as, acc) => append(as, acc))

  // exercise 3.16
  def incrementAll(ns: List[Int]): List[Int] =
    foldRight(ns, List.empty[Int]) { (n, acc) => Cons(n + 1, acc) }

  // exercise 3.17
  def toStringAll(ns: List[Double]): List[String] =
    foldRight(ns, List.empty[String]) { (n, acc) => Cons(n.toString, acc) }

  // exercise 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, List.empty[B]) { (a, acc) => Cons(f(a), acc) }

  // exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List.empty[A]) { (a, acc) =>
      if (f(a)) Cons(a, acc) else acc
    }

  def filterOutAllOddNumbers(ns: List[Int]): List[Int] = filter(ns)(_ % 2 == 0)

  // exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, List.empty[B]) { (a, acc) => append(f(a), acc) }

  // exercise 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as) { a => if (f(a)) List(a) else Nil }

  // exercise 3.22
  def pairwiseSum(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a1Head, a1Tail), Cons(a2Head, a2Tail)) =>
      Cons(a1Head + a2Head, pairwiseSum(a1Tail, a2Tail))
  }

  // exercise 3.23
  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), zipWith(at, bt)(f))
  }

  def isEmpty[A](as: List[A]) = length(as) == 0
  def nonEmpty[A](as: List[A]) = !isEmpty(as)

  // exercise 3.24 -- the book said this doesn't have to be efficient ;-)
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    if (length(sub) <= length(sup)) {
      val matchingElements = filter(zipWith(sup, sub)(_ == _))(identity)
      length(matchingElements) == length(sub) || hasSubsequence(tail(sup), sub)
    } else {
      false
    }
  }
}
