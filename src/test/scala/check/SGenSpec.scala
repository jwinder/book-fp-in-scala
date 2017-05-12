package check

import org.specs2.mutable.Specification

class SGenSpec extends Specification {
  import data._

  "listOf" in {
    val rng = RNG.simple()
    val gen = Gen.choose(1,100)
    SGen.listOf(gen)(5).sample.run(rng)._1 must_== Gen.listOfN(5, gen).sample.run(rng)._1
  }
}
