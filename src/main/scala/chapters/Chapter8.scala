package chapters

import data._
import check._
import concurrent._

object Chapter8 {

  // exercise 8.1
  case class SumIntsProperties(ints: List[Int]) {
    def findSum: List[Int] => Int = List.sum(_)
    def sum: Int = findSum(ints)

    def reverseCheck = sum == findSum(ints.reverse)
    def nilCheck = if (ints.isEmpty) sum == 0 else true

    def distinctElementCheck = {
      val size = ints.size
      lazy val head = ints.head
      if (size > 0 && ints.forall(_ == head)) sum == head * size else true
    }

    def check() = {
      reverseCheck && nilCheck && distinctElementCheck
    }
  }

  // exercise 8.2
  case class MaxIntProperties(ints: List[Int]) {
    def findMax: List[Int] => Option[Int] = _.maxOption
    def max: Option[Int] = findMax(ints)
    def unsafeMax: Int = ints.max

    def reverseCheck = max == findMax(ints.reverse)
    def emptyListCheck = if (ints.isEmpty) max.isNone else max.isSome
    def greaterThanOtherElementsCheck = ints.forall(_ <= unsafeMax)

    def check() = {
      reverseCheck && emptyListCheck && greaterThanOtherElementsCheck
    }
  }

  // exercise 8.14
  object ListSortedProperties {
    def minCheck(ints: List[Int]): Boolean = {
      if (ints.isEmpty) {
        true
      } else {
        val min = ints.head
        ints.forall(_ >= min)
      }
    }

    def maxCheck(ints: List[Int]): Boolean = {
      if (ints.isEmpty) {
        true
      } else {
        val max = ints.last
        ints.forall(_ <= max)
      }
    }

    def orderCheck(ints: List[Int]): Boolean = ints match {
      case Nil => true
      case Cons(h, t) => minCheck(t) && maxCheck(t) && orderCheck(t)
    }

    def checkList(ints: List[Int]): Boolean = {
      minCheck(ints) && maxCheck(ints) && orderCheck(ints)
    }

    def runProps(): Prop.Result = {
      val gen = SGen.nonEmptyListOf(Gen.choose(1,100)).map(_.sorted)
      val prop = Prop.forAll(gen)(checkList)
      Prop.run(prop, 100, 100)
    }

    def check() = runProps().isPassed
  }

  // exercise 8.17
  object ForkXEqualsXProperties {

    def runProps(): Prop.Result = {
      val gen = (Gen.double ** Gen.executorService).map {
        case (n, es) => (n, Par.lazyUnit(n), es)
      }
      val prop = Prop.forAll(gen) {
        case (n, parN, es) => n == parN(es).get()
      }
      Prop.run(prop, 100, 100)
    }

    def check() = runProps().isPassed
  }

  // exercise 8.18
  object TakeWhileProperties {

    def check() = {
      val gen = SGen.nonEmptyListOf(Gen.choose(1,100)).map(_.distinct.sorted)
      def isPassed(check: List[Int] => Boolean): Boolean = {
        val prop = Prop.forAll(gen)(check)
        Prop.run(prop, 100, 100).isPassed
      }

      isPassed(ints => ints.takeWhile(_ => true) == ints) &&
        isPassed(ints => ints.takeWhile(_ => false) == Nil) &&
        isPassed(ints => ints.takeWhile(_ < ints.last) == ints.init) &&
        isPassed(ints => ints.takeWhile(_ < ints.head) == Nil)
    }
  }
}
