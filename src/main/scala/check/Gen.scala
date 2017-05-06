package check

import data._

case class Gen[+A](sample: State[RNG, A])

object Gen {
  // exercise 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(Rand.randomIntBetween(start, stopExclusive)))
  }
}
