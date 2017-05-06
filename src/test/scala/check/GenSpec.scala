package check

import org.specs2.mutable.Specification
import scala.util.Random

class GenSpec extends Specification {
  import data._

  def simpleRNGWithRandomSeed() = SimpleRNG(Random.nextLong)

  "choose" in {
    val rng = simpleRNGWithRandomSeed()
    val (i, rng2) = Gen.choose(1,100).sample.run(rng)
    i >= 1 must beTrue
    i < 100 must beTrue
  }
}
