package chapters

import org.specs2.mutable.Specification
import scala.util.Random

class Chapter6Spec extends Specification {
  import Chapter6._
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
    nonNegativeInt(rng)._1 >= 0 must beTrue
  }

  "randomDouble" in {
    val rng = simpleRNGWithRandomSeed()
    val (d, rng2) = randomDouble(rng)
    d >= 0.0 && d < 1.0 must beTrue
  }

  "Rand.int" in {
    val rng = simpleRNGWithRandomSeed()
    Rand.int(rng)._1 must_== rng.nextInt._1
  }

  "Rand.unit" in {
    val rng = simpleRNGWithRandomSeed()
    Rand.unit(5)(rng) must_== (5, rng)
  }

  "Rand.map" in {
    val rng = simpleRNGWithRandomSeed()
    val str = Rand.map(Rand.int)(_.toString)
    str(rng)._1 must_== Rand.int(rng)._1.toString
  }

  "Rand.double" in {
    val rng = simpleRNGWithRandomSeed()
    val d = Rand.double(rng)._1
    d >= 0.0 && d < 1.0 must beTrue
  }

  "Rand.both" in {
    val rng = simpleRNGWithRandomSeed()
    val (int, str) = Rand.both(Rand.int, Rand.map(Rand.int)(_.toString))(rng)._1
    int must_== Rand.int(rng)._1
    str must_== Rand.int(rng)._1.toString

    val ((i, d), rng2) = Rand.randIntDouble(rng)
    val (d2, i2) = Rand.randDoubleInt(rng)._1
    i must_== Rand.int(rng)._1
    d must_== Rand.double(rng)._1
    i must_== i2
    d must_== d2

    Rand.int(rng2) must_!== Rand.int(rng)
  }

  "Rand.sequence" in {
    val rng = simpleRNGWithRandomSeed()
    val rands = List(Rand.int, Rand.double)

    val (i, rng2) = Rand.int(rng)
    val (d, _) = Rand.double(rng2)
    Rand.sequence(rands)(rng)._1 must_== List(i, d)
  }

  "candy machine - inserting a coin into a locked machine causes an unlock if there is candy left" in {
    Candy.Machine(true, 10, 10).input(Candy.Coin) must_== Candy.Machine(false, 10, 11)
    Candy.Machine(true, 0, 10).input(Candy.Coin) must_== Candy.Machine(true, 0, 10)
  }

  "candy machine - a turn on an unlocked machine causes candy to dispense and become locked" in {
    Candy.Machine(false, 10, 10).input(Candy.Turn) must_== Candy.Machine(true, 9, 10)
    Candy.Machine(true, 10, 10).input(Candy.Turn) must_== Candy.Machine(true, 10, 10)
  }

  "candy machine - ignore all inputs when in the wrong states" in {
    Candy.Machine(true, 10, 10).input(Candy.Turn) must_== Candy.Machine(true, 10, 10)
    Candy.Machine(false, 10, 10).input(Candy.Coin) must_== Candy.Machine(false, 10, 10)
    Candy.Machine(true, 0, 10).input(Candy.Turn) must_== Candy.Machine(true, 0, 10)
    Candy.Machine(true, 0, 10).input(Candy.Coin) must_== Candy.Machine(true, 0, 10)
  }

  "candy machine - simulateMachine" in {
    val inputs = List(Candy.Coin, Candy.Turn, Candy.Coin, Candy.Turn)
    Candy.simulateMachine(inputs).run(Candy.Machine(true, 5, 10)) must_== ((12, 3), Candy.Machine(true, 3, 12))
  }
}
