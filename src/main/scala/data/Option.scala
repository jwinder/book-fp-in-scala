package data

sealed trait Option[+A] {
  import Option._

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
}
