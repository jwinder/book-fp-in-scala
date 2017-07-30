package data

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B

  def foldMap[A,B](as: F[A])(f: A => B)(mB: Monoid[B]): B = foldLeft(as)(mB.zero)((b,a) => mB.op(b,f(a)))
  def concatenate[A](as: F[A])(mA: Monoid[A]): A = foldLeft(as)(mA.zero)(mA.op)

  // exercise 10.15
  def toList[A](as: F[A]): List[A] = foldRight(as)(List.empty[A])(Cons(_, _))
}

object Foldable {

  // exercise 10.12
  val foldableList = new Foldable[List] {
    def foldRight[A,B](as: List[A])(z: B)(f: (A,B) => B): B = as.foldRight(z)(f)
    def foldLeft[A,B](as: List[A])(z: B)(f: (B,A) => B): B = as.foldLeft(z)(f)
  }
  val foldableIndexedSeq = new Foldable[IndexedSeq] {
    def foldRight[A,B](as: IndexedSeq[A])(z: B)(f: (A,B) => B): B = as.foldRight(z)(f)
    def foldLeft[A,B](as: IndexedSeq[A])(z: B)(f: (B,A) => B): B = as.foldLeft(z)(f)
  }
  val foldableStream = new Foldable[Stream] {
    def foldRight[A,B](as: Stream[A])(z: B)(f: (A,B) => B): B = as.toScala.foldRight(z)(f)
    def foldLeft[A,B](as: Stream[A])(z: B)(f: (B,A) => B): B = as.toScala.foldLeft(z)(f)
  }

  // exercise 10.13
  val foldableTree = new Foldable[Tree] {
    def foldRight[A,B](as: Tree[A])(z: B)(f: (A,B) => B): B = as match {
      case Leaf(value) => f(value,z)
      case Branch(left, right) =>
        val r = foldRight(right)(z)(f)
        foldRight(left)(r)(f)
    }
    def foldLeft[A,B](as: Tree[A])(z: B)(f: (B,A) => B): B = as match {
      case Leaf(value) => f(z,value)
      case Branch(left, right) =>
        val r = foldLeft(left)(z)(f)
        foldLeft(right)(r)(f)
    }
  }

  // exercise 10.14
  val foldableOption = new Foldable[Option] {
    def foldRight[A,B](as: Option[A])(z: B)(f: (A,B) => B): B = as match {
      case None => z
      case Some(a) => f(a,z)
    }
    def foldLeft[A,B](as: Option[A])(z: B)(f: (B,A) => B): B = as match {
      case None => z
      case Some(a) => f(z,a)
    }
  }
}
