package check

import data._

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  // exercise 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap { n => Gen.listOfN(n, this) }

  def stream(rng: RNG): Stream[A] = {
    Stream.unfold(rng)(rng => Some(sample.run(rng)))
  }
}

object Gen {

  def fromStateFn[A](run: RNG => (A,RNG)): Gen[A] = new Gen(State(run))

  // exercise 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen.fromStateFn {
    Rand.randomIntBetween(start, stopExclusive)
  }

  // exercise 8.5

  def unit[A](a: => A): Gen[A] = Gen.fromStateFn {
    lazy val cachedA = a
    rng => (cachedA, rng)
  }

  def boolean: Gen[Boolean] = choose(0,2).map(_ % 2 == 0)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen.fromStateFn {
    Rand.sequence(List.fill(n)(g.sample.run))
  }

  // exercise 8.7
  def union[A](left: Gen[A], right: Gen[A]): Gen[A] = {
    boolean.flatMap { if (_) left else right }
  }

  // exercise 8.8

  def double: Gen[Double] = Gen.fromStateFn(Rand.double)

  def weighted[A](left: (Gen[A], Double), right: (Gen[A], Double)): Gen[A] = {
    val leftWeight = left._2.abs
    val rightWeight = right._2.abs
    val middle = leftWeight / (leftWeight + rightWeight)
    double.flatMap { d => if (d < middle) left._1 else right._1 }
  }
}
