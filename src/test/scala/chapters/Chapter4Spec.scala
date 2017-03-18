package chapters
import org.specs2.mutable.Specification

class Chapter4Spec extends Specification {
  import Chapter4._
  import data.Option

  "failingFn not RT, eg exception throwing" in {
    failingFn(1) must throwA[Exception]
  }

  "failingFn not RT, eg exception catch when value substituted" in {
    failingFn2(1) must_== 43
  }

  "mean, using Option to represent failure" in {
    mean(List(3,4,5,6,7)) must_== Option.some(5)
    mean(Nil) must_== Option.none
  }

  "variance" in {
    val dice: List[Double] = List(1,2,3,4,5,6)
    val expectedVar = 35.0/12
    variance(dice) must_== Option.some(expectedVar)
  }
}
