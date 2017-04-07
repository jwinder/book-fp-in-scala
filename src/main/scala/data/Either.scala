package data

sealed trait Either[+E, +A] {
  import Either._

  def isRight = this match {
    case Right(_) => true
    case Left(_) => false
  }
  def isLeft = !isRight

  // exercise 4.6

  def map[B](f: A => B): Either[E,B] = this match {
    case Right(a) => right(f(a))
    case Left(e) => left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => right(a)
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] =
    flatMap { a => b.map { b => f(a,b) } }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def left[E,A](value: E): Either[E,A] = Left(value)
  def right[E,A](value: A): Either[E,A] = Right(value)

  // exercise 4.7

  def traverse[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] = {
    as.foldLeft(right[E, List[B]](List.empty)) { case (acc, next) =>
      if (acc.isLeft) {
        acc
      } else {
        f(next) match {
          case Left(value) => left(value)
          case Right(value) => acc.map { accList => Cons(value, accList) }
        }
      }
    }.map(_.reverse)
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E, List[A]] = traverse(es)(identity)

  // exercise 4.8
  // Accumulate a list of errors instead of the first error.
  // For example: Either[List[E], List[A]] or Either[NonEmptyList[E], NonEmptyList[A]]
}
