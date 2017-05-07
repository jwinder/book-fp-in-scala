package data

import org.specs2.mutable.Specification

class RNGSpec extends Specification {
  import data._

  "SimpleRNG" in {
    val rng1 = RNG.simple()
    val (i1, rng2) = rng1.nextInt
    val (i2, _) = rng1.nextInt

    i1 must_== i2
    i1 must_!= rng2.nextInt
  }

  "nonNegativeInt" in {
    val rng = RNG.simple()
    RNG.nonNegativeInt(rng)._1 >= 0 must beTrue
  }

  "randomDouble" in {
    val rng = RNG.simple()
    val (d, rng2) = RNG.randomDouble(rng)
    d >= 0.0 && d < 1.0 must beTrue
  }
}
