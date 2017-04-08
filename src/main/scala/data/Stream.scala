package data

sealed trait Stream[+A] {
  import Stream._

  def equiv[A](other: Stream[A]): Boolean = (this, other) match {
    case (StreamEmpty, StreamEmpty) => true
    case (StreamCons(a, atail), StreamCons(b, btail)) => a() == b() && atail().equiv(btail())
    case _ => false
  }

  def isEmpty: Boolean = this match {
    case StreamEmpty => true
    case _ => false
  }

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
  def takeWhile(p: A => Boolean): Stream[A] = this match {
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

  // exercise 5.5
  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(empty[A]) { (next, stream) =>
    if (p(next)) cons(next, stream) else empty
  }

  // exercise 5.6
  def headOption2: Option[A] = foldRight(Option.none[A]) { (next, stream) => Some(next) }

  // exercise 5.7, some functions via foldRight
  def map[B](f: A => B): Stream[B] = foldRight(empty[B]) { (head, tail) => cons(f(head), tail) }
  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A]) { (head, tail) => if (f(head)) cons(head, tail) else tail }
  def append[B >: A](other: Stream[B]): Stream[B] = foldRight(other) { (head, tail) => cons(head, tail) }
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B]) { (head, tail) => f(head).append(tail) }

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  def tail: Stream[A] = drop(1)

  def zipWith[B,C](other: Stream[B])(f: (A,B) => C): Stream[C] = (this, other) match {
    case (StreamEmpty, _) => empty
    case (_, StreamEmpty) => empty
    case (StreamCons(a, atail), StreamCons(b, btail)) => cons(f(a(),b()), atail().zipWith(btail())(f))
  }

  // exercise 5.13, some functions via unfold

  def map2[B](f: A => B): Stream[B] = unfold(this) {
    case StreamEmpty => None
    case StreamCons(h, t) => Some((f(h()), t()))
  }

  def take2(n: Int): Stream[A] = unfold((this,n)) { case (stream, n) =>
    if (n <= 0) None else stream.headOption.map((_, (stream.tail, n-1)))
  }

  def takeWhile3(f: A => Boolean): Stream[A] = unfold(this) {
    case StreamEmpty => None
    case StreamCons(h, t) if f(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWith2[B,C](other: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, other)) {
    case ((StreamEmpty, _)) => None
    case ((_, StreamEmpty)) => None
    case ((StreamCons(a, atail), StreamCons(b, btail))) => Some((f(a(),b()), (atail(), btail())))
  }

  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, other)) {
    case ((StreamEmpty, StreamEmpty)) => None
    case ((StreamEmpty, StreamCons(b, btail))) => Some(( (None, Some(b())) , (empty, btail()) ))
    case ((StreamCons(a, atail), StreamEmpty))  => Some(( (Some(a()), None) , (atail(), empty) ))
    case ((StreamCons(a, atail), StreamCons(b, btail))) => Some(( (Some(a()), Some(b())) , (atail(), btail()) ))
  }

  // exercise 5.14
  def startsWith[A](other: Stream[A]): Boolean = {
    zipAll(other)
      .takeWhile { case (a,b) => b.isSome }
      .forAll { case (a,b) => a == b }
  }

  // exercise 5.15
  def tails: Stream[Stream[A]] = unfold(this) { stream =>
    if (stream.isEmpty) {
      None
    } else {
      Some((stream, stream.tail))
    }
  }.append(Stream(empty[A]))

  def hasSubsequence[A](other: Stream[A]): Boolean = tails.exists(_.startsWith(other))

  // exercise 5.16

  def scanRight2[B](z: B)(f: (A, => B) => B): Stream[B] = tails.map(_.foldRight(z)(f))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight((Stream(z), z)) { (next, acc) =>
    // cache values for single executation
    lazy val accStream = acc._1
    lazy val accValue = acc._2
    val nextResult = f(next, accValue)
    (cons(nextResult, accStream), nextResult)
  }._1
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

  // example from section 5.7, infinite stream of ones
  val ones: Stream[Int] = cons(1, ones)
  def fiveOnes(): List[Int] = ones.take(5).toList

  // exercise 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // exercise 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def pairwiseSum(a: Stream[Int], b: Stream[Int]) = a.zipWith(b)(_ + _)

  // exercise 5.10
  val fibs: Stream[Int] = cons(0, cons(1, pairwiseSum(fibs, fibs.tail)))

  // exercise 5.11
  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = {
    f(z).map { case (value, state) => cons(value, unfold(state)(f)) } getOrElse empty
  }

  // exercise 5.12
  def fibs2: Stream[Int] = unfold((0,1)) { case (a,b) => Some((a,(b,a+b))) }
  def from2(n: Int): Stream[Int] = unfold(n)(n => Some((n,n+1)))
  def constant2[A](a: A): Stream[A] = unfold(a)(a => Some((a,a)))
  def ones2: Stream[Int] = unfold(1)(n => Some((n,n)))
}
