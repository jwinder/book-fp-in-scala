package check

import data._

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(size: Int): Gen[A] = forSize(size)

  // exercise 8.11
  def map[B](f: A => B): SGen[B] = SGen { size =>
    forSize(size).map(f)
  }
  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen { size =>
    forSize(size).flatMap { a => f(a).forSize(size) }
  }
}

object SGen {
  def apply[A](gen: Gen[A]): SGen[A] = SGen(_ => gen)

  // exercise 8.12
  def listOf[A](gen: Gen[A]): SGen[List[A]] = SGen { size =>
    Gen.listOfN(size, gen)
  }

  // exercise 8.13
  def nonEmptyListOf[A](gen: Gen[A]): SGen[List[A]] = SGen { size =>
    Gen.listOfN(math.max(size, 1), gen)
  }
}
