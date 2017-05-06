package data

import org.specs2.mutable.Specification
import scala.util.Random

class RNGSpec extends Specification {
  import data._

  def simpleRNGWithRandomSeed() = SimpleRNG(Random.nextLong)

  "SimpleRNG" in {
    val rng1 = simpleRNGWithRandomSeed()
    val (i1, rng2) = rng1.nextInt
    val (i2, _) = rng1.nextInt

    i1 must_== i2
    i1 must_!= rng2.nextInt
  }

  "nonNegativeInt" in {
    val rng = simpleRNGWithRandomSeed()
    RNG.nonNegativeInt(rng)._1 >= 0 must beTrue
  }

  "randomDouble" in {
    val rng = simpleRNGWithRandomSeed()
    val (d, rng2) = RNG.randomDouble(rng)
    d >= 0.0 && d < 1.0 must beTrue
  }
}
