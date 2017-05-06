package data

trait RNG {
  def nextInt: (Int, RNG)
  def peekNextSeed: Long
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = peekNextSeed()
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def peekNextSeed(): Long = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
}

object RNG {
  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  // exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, rng2) = rng.nextInt
    val nonNegativeInt = math.abs(int)
    val result = if (nonNegativeInt == Int.MinValue) nonNegativeInt - 1 else nonNegativeInt
    (result, rng2)
  }

  // exercise 6.2
  def randomDouble(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    val d = i.toDouble / (Int.MaxValue.toDouble + 1)
    (d, rng2)
  }

  // exercise 6.3

  def randomIntDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = randomDouble(rng2)
    ((i, d), rng3)
  }

  def randomDoubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = randomIntDouble(rng)
    ((d, i), rng2)
  }

  def randomDouble3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = randomDouble(rng)
    val (d2, rng3) = randomDouble(rng2)
    val (d3, rng4) = randomDouble(rng3)
    ((d1, d2, d3), rng4)
  }

  // exercise 6.4
  def randomInts(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) {
      (List.empty, rng)
    } else {
      val (int, rng2) = rng.nextInt
      val (ints, lastRNG) = randomInts(count - 1)(rng2)
      (Cons(int, ints), lastRNG)
    }
  }
}
