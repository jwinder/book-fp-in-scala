package data
import scala.annotation.tailrec
import scala.collection.immutable.{List => ScalaList}

sealed trait List[+A] {
  def getAtIndex(index: Int): Option[A] = List.getAtIndex(this, index)
  def get(index: Int): Option[A] = getAtIndex(index)
  def valueAtIndex(index: Int): A = List.valueAtIndex(this, index)
  def apply(index: Int): A = valueAtIndex(index)
  def tail: List[A] = List.tail(this)
  def setHead[A](head: A) = List.setHead(head, this)
  def drop(n: Int): List[A] = List.drop(this, n)
  def dropWhile(f: A => Boolean): List[A] = List.dropWhile(this, f)
  def init: List[A] = List.init(this)
  def foldLeft[B](z: B)(f: (B,A) => B): B = List.foldLeft(this, z)(f)
  def reverse: List[A] = List.reverse(this)
  def foldRight[B](z: B)(f: (A,B) => B): B = List.foldRight(this, z)(f)
  def append[A](other: List[A]) = List.append(this, other)
  def appendElement[A](a: A) = List.appendElement(this, a)
  def appendAll[A](lists: List[List[A]]) = List.appendAll(Cons(this, lists))
  def map[B](f: A => B): List[B] = List.map(this)(f)
  def filter(f: A => Boolean): List[A] = List.filter(this)(f)
  def flatMap[B](f: A => List[B]): List[B] = List.flatMap(this)(f)
  def zipWith[B,C](bs: List[B])(f: (A,B) => C): List[C] = List.zipWith(this, bs)(f)
  def isEmpty: Boolean = List.isEmpty(this)
  def nonEmpty: Boolean = List.nonEmpty(this)
  def hasSubsequence[A](sub: List[A]): Boolean = List.hasSubsequence(this, sub)
  def length: Int = List.length(this)
  def size: Int = List.size(this)
  def sorted[B >: A](implicit ordering: Ordering[B]): List[A] = List.sorted[A,B](this)(ordering)
  def forall(p: A => Boolean): Boolean = List.forall(this)(p)
  def foreach(f: A => Unit): Unit = List.foreach(this)(f)
  def headOption: Option[A] = List.headOption(this)
  def head: A = List.head(this)
  def lastOption: Option[A] = List.lastOption(this)
  def last: A = List.last(this)
  def minOption[B >: A](implicit ordering: Ordering[B]): Option[A] = List.minOption[A,B](this)(ordering)
  def min[B >: A](implicit ordering: Ordering[B]): A = List.min[A,B](this)(ordering)
  def maxOption[B >: A](implicit ordering: Ordering[B]): Option[A] = List.maxOption[A,B](this)(ordering)
  def max[B >: A](implicit ordering: Ordering[B]): A = List.max[A,B](this)(ordering)
  def exists(p: A => Boolean): Boolean = List.exists(this)(p)
  def contains[B >: A](a: B): Boolean = List.contains[A,B](this)(a)
  def take(n: Int): List[A] = List.take(this)(n)
  def takeWhile(f: A => Boolean): List[A] = List.takeWhile(this)(f)
  def distinct: List[A] = List.distinct(this)

  // couldn't quite get the variance right on reduce, taking the easy way out
  def toScala(): ScalaList[A] = List.toScala(this)
  def reduce[B >: A](f: (B,B) => B): B = toScala.reduce(f)
  def reduceLeft[B >: A](f: (B,A) => B): B = toScala.reduceLeft(f)
}

case object Nil extends List[Nothing]
case class Cons[+A](theHead: A, theTail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  }

  def empty[A]: List[A] = Nil

  def fromScala[A](as: ScalaList[A]): List[A] = apply(as: _*)
  def toScala[A](as: List[A]): ScalaList[A] = {
    as.foldLeft(ScalaList.empty[A]) { case (sl, a) => sl :+ a }
  }

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
  def init[A](as: List[A]): List[A] = {
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

  def size[A](as: List[A]) = length(as)

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

  def appendElement[A](as: List[A], a: A): List[A] = append(as, List(a))

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

  // some extra functions used in further chapters

  def fill[A](n: Int)(a: => A): List[A] = if (n <= 0) {
    Nil
  } else {
    Cons(a, fill(n-1)(a))
  }

  def getAtIndex[A](as: List[A], index: Int): Option[A] = as match {
    case Nil => Option.none[A]
    case Cons(head, tail) if index <= 0 => Option.some(head)
    case Cons(head, tail) => getAtIndex(tail, index - 1)
  }

  def valueAtIndex[A](as: List[A], index: Int): A = as match {
    case Nil => throw new IndexOutOfBoundsException
    case Cons(head, tail) if index <= 0 => head
    case Cons(head, tail) => valueAtIndex(tail, index - 1)
  }

  def sorted[A, B >: A](as: List[A])(implicit ordering: Ordering[B]): List[A] = {
    val length = as.length
    if (length > 0) {
      val middle = as.valueAtIndex(length/2)
      val lessThan: List[A] = as.filter(a => ordering.compare(a, middle) < 0).sorted(ordering)
      val equals: List[A] = as.filter(a => ordering.compare(a, middle) == 0)
      val greaterThan: List[A] = as.filter(a => ordering.compare(a, middle) > 0).sorted(ordering)
      List.appendAll(List(lessThan, equals, greaterThan))
    } else {
      as
    }
  }

  def forall[A](as: List[A])(p: A => Boolean): Boolean = foldLeft(as, true)(_ && p(_))

  def foreach[A](as: List[A])(f: A => Unit): Unit = as match {
    case Nil => ()
    case Cons(h, t) =>
      f(h)
      foreach(t)(f)
  }

  def headOption[A](as: List[A]): Option[A] = as.getAtIndex(0)

  def head[A](as: List[A]): A = try {
    as.valueAtIndex(0)
  } catch {
    case t: IndexOutOfBoundsException => throw new IndexOutOfBoundsException("Head of empty list")
  }

  def lastOption[A](as: List[A]): Option[A] = as.reverse.headOption
  def last[A](as: List[A]): A = try {
    as.reverse.head
  } catch {
    case t: IndexOutOfBoundsException => throw new IndexOutOfBoundsException("Last of empty list")
  }

  def minOption[A, B >: A](as: List[A])(implicit ordering: Ordering[B]): Option[A] = as.sorted(ordering).headOption
  def min[A, B >: A](as: List[A])(implicit ordering: Ordering[B]): A = as.sorted(ordering).head
  def maxOption[A, B >: A](as: List[A])(implicit ordering: Ordering[B]): Option[A] = as.sorted(ordering.reverse).headOption
  def max[A, B >: A](as: List[A])(implicit ordering: Ordering[B]): A = as.sorted(ordering.reverse).head

  def exists[A](as: List[A])(p: A => Boolean): Boolean = as match {
    case Cons(h, t) if p(h) => true
    case Cons(h, t) => exists(t)(p)
    case Nil => false
  }

  def contains[A, B >: A](as: List[A])(a: B): Boolean = as.exists(_ == a)

  def take[A](as: List[A])(n: Int): List[A] = as match {
    case Nil => Nil
    case Cons(head, tail) if n > 0 => Cons(head, take(tail)(n - 1))
    case other => Nil
  }

  def takeWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(head, tail) if f(head) => Cons(head, takeWhile(tail)(f))
    case other => Nil
  }

  def distinct[A](as: List[A]): List[A] = as.foldRight(List.empty[A]) {
    (head, tail) => Cons(head, tail.filter(_ != head))
  }
}
