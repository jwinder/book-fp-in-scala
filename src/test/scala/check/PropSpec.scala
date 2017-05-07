package check

import org.specs2.mutable.Specification

class PropSpec extends Specification {
  import data._

  "check" in {
    val rng = RNG.simple()
    Prop.check(true).run(10,10,rng).isPassed must beTrue
    Prop.check(false).run(10,10,rng).isFalsified must beTrue
  }

  "run" in {
    val gen = Gen.choose(1,100)
    Prop.run(Prop.forAll(gen)(_ < 100), 10, 10).isPassed must beTrue
    Prop.run(Prop.forAll(gen)(_ > 100), 10, 10).isFalsified must beTrue
  }

  "forAll" in {
    val rng = RNG.simple()
    val gen = Gen.choose(1,100)
    Prop.forAll(gen)(_ < 100).run(10, 10, rng).isPassed must beTrue
    Prop.forAll(gen)(_ > 100).run(10, 10, rng).isFalsified must beTrue
  }

  "&& and ||" in {
    val rng = RNG.simple()
    val gen = Gen.choose(1,2)
    val passingProp = Prop.forAll(gen)(_ == 1)
    val falsifyingProp = Prop.forAll(gen)(_ > 1)

    Prop.run(passingProp, 10, 1).isPassed must beTrue
    Prop.run(falsifyingProp, 10, 1).isFalsified must beTrue

    Prop.run(passingProp || falsifyingProp, 10, 1).isPassed must beTrue
    Prop.run(passingProp && falsifyingProp, 10, 1).isFalsified must beTrue
    Prop.run(falsifyingProp|| passingProp, 10, 1).isPassed must beTrue
    Prop.run(falsifyingProp && passingProp, 10, 1).isFalsified must beTrue
  }
}
