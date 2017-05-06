package chapters

import data._

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
    def unsafeMax: Int = max.getOrElse(throw new IndexOutOfBoundsException("Max of empty list"))

    def reverseCheck = max == findMax(ints.reverse)
    def emptyListCheck = if (ints.isEmpty) max.isNone else max.isSome
    def greaterThanOtherElementsCheck = ints.forall(_ <= unsafeMax)

    def check() = {
      reverseCheck && emptyListCheck && greaterThanOtherElementsCheck
    }
  }
}
