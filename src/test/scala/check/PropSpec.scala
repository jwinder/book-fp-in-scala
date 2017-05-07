package check

import org.specs2.mutable.Specification
import scala.util.Random

class PropSpec extends Specification {
  import data._

  def simpleRNGWithRandomSeed() = SimpleRNG(Random.nextLong)

  "forAll" in {
    val rng = simpleRNGWithRandomSeed()
    val gen = Gen.choose(1,100)
    Prop.forAll(gen)(_ < 100).run(1000, rng).isFalsified must beFalse
    Prop.forAll(gen)(_ > 100).run(1000, rng).isFalsified must beTrue
  }
}
