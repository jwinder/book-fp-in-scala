package data

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = List.append(a1, a2)
    def zero: List[A] = List.empty[A]
  }

  // exercise 10.1

  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero: Boolean = true
  }

  // exercise 10.2

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    def zero: Option[A] = Option.empty[A]
  }

  // exercise 10.3

  def endoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1.andThen(a2)
    def zero: A => A = identity
  }

  // exercise 10.4

  import check._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    Prop.forAll(gen ** gen ** gen) {
      case ((a,b),c) =>
        m.op(a, m.zero) == a && m.op(m.zero, a) == a &&
        m.op(m.op(a,b), c) == m.op(a, m.op(b,c))
    }
  }

  // exercise 10.5
  def foldMap[A,B](as: List[A], mb: Monoid[B])(f: A => B): B = {
    as.foldLeft(mb.zero) { (b, a) => mb.op(b, f(a)) }
  }

  // exercise 10.6
  def foldLeftViaFoldMap[A,B](as: List[A], z: B)(f: (B,A) => B): B = {
    val mb: Monoid[B => B] = endoMonoid[B]
    foldMap(as, mb)(a => b => f(b,a))(z)
  }

  // exercise 10.7
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    v.size match {
      case 0 => m.zero
      case 1 => f(v(0))
      case size =>
        val middle = size / 2
        val fst = v.take(middle)
        val lst = v.drop(middle)
        m.op(foldMapV(fst, m)(f), foldMapV(lst, m)(f))
    }
  }

  // exercise 10.8

  object par {
    import concurrent._
    import Par._

    def apply[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
      def zero: Par[A] = lazyUnit(m.zero)
      def op(left: Par[A], right: Par[A]): Par[A] = map2(left, right)(m.op)
    }

    def foldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
      val parM: Monoid[Par[B]] = par(m)
      def _foldMap(v: IndexedSeq[A]): Par[B] = {
        v.size match {
          case 0 => parM.zero
          case 1 => Par.map(lazyUnit(v(0)))(f)
          case size =>
            val middle = size / 2
            val fst = v.take(middle)
            val lst = v.drop(middle)
            parM.op(_foldMap(fst), _foldMap(lst))
        }
      }
      _foldMap(v)
    }
  }

  // exercise 10.16
  def productMonoid[A,B](mA: Monoid[A], mB: Monoid[B]): Monoid[(A,B)] = new Monoid[(A,B)] {
    def op(a1: (A,B), a2: (A,B)): (A,B) = (mA.op(a1._1, a2._1), mB.op(a1._2, a2._2))
    def zero: (A,B) = (mA.zero, mB.zero)
  }

  def mapMergeMonoid[K,V](mV: Monoid[V]): Monoid[Map[K,V]] = new Monoid[Map[K,V]] {
    def op(a: Map[K,V], b: Map[K,V]): Map[K,V] = {
      (a.keySet ++ b.keySet).foldLeft(zero) { (map, key) =>
        val value = mV.op(a.getOrElse(key, mV.zero), b.getOrElse(key, mV.zero))
        map.updated(key, value)
      }
    }
    def zero = Map.empty[K,V]
  }

  // exercise 10.17
  def functionMonoid[A,B](mB: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(a1: A => B, a2: A => B): A => B = a => mB.op(a1(a), a2(a))
    def zero: A => B = _ => mB.zero
  }
}
