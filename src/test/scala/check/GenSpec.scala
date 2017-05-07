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

  "unit" in {
    Gen.unit(5).sample.run(simpleRNGWithRandomSeed())._1 must_== 5
  }

  "boolean" in {
    val bool = Gen.boolean.sample.run(simpleRNGWithRandomSeed())._1
    (bool must beTrue) or (bool must beFalse)
  }

  "double" in {
    val rng = simpleRNGWithRandomSeed()
    val d = Gen.double.sample.run(rng)._1
    d >= 0.0 && d < 1.0 must beTrue
  }

  "listOfN" in {
    val rng = simpleRNGWithRandomSeed()
    val gen = Gen.choose(1,100)
    val ints = Gen.listOfN(5, gen).sample.run(rng)._1
    ints.size must_== 5
    ints.forall(n => n >= 1 && n < 100) must beTrue

    gen.listOfN(Gen.unit(5)).sample.run(rng)._1 must_== ints
  }

  "flatMap" in {
    val rng = simpleRNGWithRandomSeed()
    Gen.unit(5).flatMap(n => Gen.unit(2*n)).sample.run(rng)._1 must_== 10
  }

  "union" in {
    val rng = simpleRNGWithRandomSeed()
    val int = Gen.union(Gen.unit(1), Gen.unit(2)).sample.run(rng)._1
    (int must_== 1) or (int must_== 2)
  }

  "stream" in {
    val rng = simpleRNGWithRandomSeed()
    val streamedList = Gen.choose(1,100).stream(rng).take(2).toList
    val (i, rng2) = Rand.randomIntBetween(1,100)(rng)
    val (i2, _) = Rand.randomIntBetween(1,100)(rng2)
    streamedList must_== List(i, i2)
  }
}
