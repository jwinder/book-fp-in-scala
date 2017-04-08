package data

sealed trait Stream[+A] {
  import Stream._

  def equiv[A](other: Stream[A]) = this.toList == other.toList

  def isEmpty: Boolean = equiv(Stream.empty)
  def nonEmpty: Boolean = !isEmpty

  def headOption: Option[A] = this match {
    case StreamEmpty => None
    case StreamCons(h, _) => Some(h())
  }

  // exercise 5.1
  def toList: List[A] = this match {
    case StreamEmpty => Nil
    case StreamCons(h, t) => Cons(h(), t().toList)
  }

  // exercise 5.2

  def take(n: Int): Stream[A] = if (n <= 0) {
    empty
  } else {
    this match {
      case StreamEmpty => empty
      case StreamCons(h, t) => cons(h(), t().take(n - 1))
    }
  }

  def drop(n: Int): Stream[A] = if (n <= 0) {
    this
  } else {
    this match {
      case StreamEmpty => empty
      case StreamCons(_, t) => t().drop(n - 1)
    }
  }

  // exercise 5.3
  def takeWhile2(p: A => Boolean): Stream[A] = this match {
    case StreamEmpty => empty
    case StreamCons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
    case other => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case StreamCons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a,b) => p(a) || b)

  // exercise 5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case StreamCons(h,t) => p(h()) && t().forAll(p)
    case _ => true
  }

  // exercise 5.5, takeWhile via foldRight
  def takeWhile(p: A => Boolean): Stream[A] = ???
}

case object StreamEmpty extends Stream[Nothing]
case class StreamCons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    // cache values to prevent multiple execution
    lazy val head = h
    lazy val tail = t
    StreamCons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = StreamEmpty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
}
