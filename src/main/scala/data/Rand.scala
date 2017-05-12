package data

object Rand {
  import RNG._

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(n => n - n % 2)

  // exercise 6.5
  def double: Rand[Double] = map(nonNegativeInt) { n =>
    n.toDouble / (Int.MaxValue.toDouble + 1)
  }

  // exercise 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = rng => {
    map(ra) { a =>
      map(rb) { b =>
        f(a,b)
      }(rng)._1
    }(rng)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))
  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    val (list, finalRNG) = fs.foldRight((List.empty[A], rng)) {
      case (f, (as, lastRNG)) =>
        val (nextValue, nextRNG) = f(lastRNG)
        (Cons(nextValue, as), nextRNG)
    }
    (list, finalRNG)
  }

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  // exercise 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  // exercise 6.9
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))
  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))

  def randomIntBetween(start: Int, stopExclusive: Int): Rand[Int] = {
    map(nonNegativeInt) { i => start + i % (stopExclusive - start) }
  }
}
