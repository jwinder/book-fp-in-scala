package data

import org.specs2.mutable.Specification

class RandSpec extends Specification {
  import data._

  "Rand.int" in {
    val rng = RNG.simple()
    Rand.int(rng)._1 must_== rng.nextInt._1
  }

  "Rand.unit" in {
    val rng = RNG.simple()
    Rand.unit(5)(rng) must_== (5, rng)
  }

  "Rand.map" in {
    val rng = RNG.simple()
    val str = Rand.map(Rand.int)(_.toString)
    str(rng)._1 must_== Rand.int(rng)._1.toString
  }

  "Rand.double" in {
    val rng = RNG.simple()
    val d = Rand.double(rng)._1
    d >= 0.0 && d < 1.0 must beTrue
  }

  "Rand.both" in {
    val rng = RNG.simple()
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
    val rng = RNG.simple()
    val rands = List(Rand.int, Rand.double)

    val (d, rng2) = Rand.double(rng)
    val (i, _) = Rand.int(rng2)
    Rand.sequence(rands)(rng)._1 must_== List(i, d)
  }
}
