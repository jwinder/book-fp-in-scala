package data

sealed trait Option[+A] {
  import Option._

  def isSome: Boolean = !(isNone)
  def isNone: Boolean = this == Option.none

  // exercise 4.1

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => some(f(a))
    case None => none
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => none
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](default: => Option[B]): Option[B] = this match {
    case Some(a) => some(a)
    case None => default
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) => if (f(a)) some(a) else none
    case None => none
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def apply[A](a: A): Option[A] = Some(a)
  def empty[A]: Option[A] = None

  def some[A](a: A): Option[A] = Some(a)
  def none[A]: Option[A] = None

  // exercise 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
    for {
      a <- a
      b <- b
    } yield f(a,b)
  }

  // exercise 4.4
  def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldLeft(some(List.empty[A])) { case (acc, next) =>
      if (acc.isNone) {
        acc
      } else {
        next match {
          case None => none
          case Some(value) => acc.map { accumulatedList => Cons(value, accumulatedList) }
        }
      }
    }.map(_.reverse)
  }

  // exercise 4.5
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldLeft(some(List.empty[B])) { case (acc, next) =>
      if (acc.isNone) {
        acc
      } else {
        f(next) match {
          case None => none
          case Some(value) => acc.map { accumulatedList => Cons(value, accumulatedList) }
        }
      }
    }.map(_.reverse)
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)
}
