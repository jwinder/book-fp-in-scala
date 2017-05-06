package check

import data._
import org.specs2.mutable.Specification

class BooleanPropSpec extends Specification {

  "&&" in {
    (BooleanProp(true) && BooleanProp(true)).check must beTrue
    (BooleanProp(true) && BooleanProp(false)).check must beFalse
    (BooleanProp(false) && BooleanProp(true)).check must beFalse
  }
}
