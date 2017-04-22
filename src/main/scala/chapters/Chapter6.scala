package chapters
import data._

object Chapter6 {

  // State is implemented in data.State

  // purely functional random number generation

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

  type Rand[+A] = RNG => (A, RNG)

  object Rand {
    val int: Rand[Int] = _.nextInt
    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

    def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(n => n - n % 2)

    // exercise 6.5
    def double: Rand[Double] = map(nonNegativeInt) { n =>
      n.toDouble / (Int.MaxValue.toDouble + 1)
    }

    // exercise 6.6
    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = rng => {
      map(ra) { a =>
        map(rb) { b =>
          f(a,b)
        }(rng)._1
      }(rng)
    }

    def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))
    val randIntDouble: Rand[(Int, Double)] = both(int, double)
    val randDoubleInt: Rand[(Double, Int)] = both(double, int)

    // exercise 6.7
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
      val (list, finalRNG) = fs.foldLeft((List.empty[A], rng)) {
        case ((as, lastRNG), f) =>
          val (nextValue, nextRNG) = f(lastRNG)
          (Cons(nextValue, as), nextRNG)
      }
      (list.reverse, finalRNG)
    }

    def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

    // exercise 6.8
    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

    def nonNegativeLessThan(n: Int): Rand[Int] =
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }

    // exercise 6.9
    def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s)(a => unit(f(a)))
    def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
      flatMap(ra)(a => map(rb)(b => f(a,b)))
  }

  // exercise 6.11
  object Candy {
    sealed trait Input
    case object Coin extends Input
    case object Turn extends Input

    case class Machine(locked: Boolean, candies: Int, coins: Int) {
      def input(input: Input): Machine = {
        if (input == Coin && locked && candies > 0) copy(locked = false, coins = coins + 1)
        else if (input == Turn && !locked) copy(locked = true, candies = candies - 1)
        else this
      }
    }

    // operate the machine based on list of inputs
    // returns the number of coins and candies left in machine at the end
    // eg. if the machine has 10 coins & 5 candies and a total of 4 candies are bought, the output should be (14, 1)
    def simulateMachine(inputs: List[Input]): State[Machine, (Int,Int)] = State { machine =>
      val updatedMachine = inputs.foldLeft(machine) { (machine, input) => machine.input(input) }
      ((updatedMachine.coins, updatedMachine.candies), updatedMachine)
    }
  }
}
