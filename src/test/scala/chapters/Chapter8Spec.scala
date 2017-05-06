package chapters

import org.specs2.mutable.Specification

class Chapter8Spec extends Specification {
  import Chapter8._
  import data._

  "SumIntsProperties" in {
    SumIntsProperties(List.empty).check() must beTrue
    SumIntsProperties(List(1,2,3,4)).check() must beTrue
  }

  "MaxIntProperties" in {
    MaxIntProperties(List.empty).check() must beTrue
    MaxIntProperties(List(1,2,3)).check() must beTrue
  }
}
