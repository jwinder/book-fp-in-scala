package check

import data._

case class Prop(run: (Prop.MaxSize, Prop.TestCases, RNG) => Prop.Result) {
  import Prop._

  // exercise 8.9

  def &&(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Passed => p.run(max, n, rng)
      case Proved => p.run(max, n, rng)
      case falsified => falsified
    }
  }

  def ||(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case falsified: Falsified =>
        p.run(max, n, rng) match {
          case falsified2: Falsified => Result.falsified(falsified, falsified2)
          case passed => passed
        }
      case passed => passed
    }
  }
}

object Prop {
  type MaxSize = Int
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  def check(p: => Boolean): Prop = Prop { (_,_,_) =>
    if (p) Passed else Falsified("()", 0)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.simple(System.currentTimeMillis)): Result = {
    val result = p.run(maxSize, testCases, rng)
    result match {
      case Falsified(report, n) =>
        println(s"! Falsified after $n passed tests:\n\n $report")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }
    result
  }

  sealed trait Result {
    def isFalsified: Boolean
    def isPassed = !isFalsified
  }

  object Result {
    def passed: Result = Passed
    def falsified(failure: FailedCase, successes: SuccessCount): Result =
      Falsified(failure, successes)
    def falsified[A](failedCase: A, exception: Exception, successes: SuccessCount): Result =
      Falsified(Falsified.buildReport(failedCase, exception), successes)
    def falsified(f1: Falsified, f2: Falsified): Result =
      Falsified(Falsified.combineReports(f1.failure, f2.failure), f1.successes + f2.successes)
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  case class Falsified(
    failure: FailedCase,
    successes: SuccessCount
  ) extends Result {
    def isFalsified = true
  }

  object Falsified {
    def buildReport[A](s: A, e: Exception): String = {
      s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
    }
    def combineReports(left: String, right: String): String = {
      left + "\n\n" + right
    }
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (_, n, rng) =>
    as.stream(rng).zip(Stream.from(0)).take(n).map { case (a, index) =>
      try {
        if (f(a)) Result.passed else Result.falsified(a.toString, index)
      } catch {
        case e: Exception => Result.falsified(a, e, index)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

  def maxProp: Prop = forAll(SGen.nonEmptyListOf(Gen.smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }
}
