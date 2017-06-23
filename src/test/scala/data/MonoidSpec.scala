package data
import org.specs2.mutable.Specification

class MonoidSpec extends Specification {
  import Monoid._

  "stringMonoid" in {
    stringMonoid.op("a", "b") must_== "ab"
    stringMonoid.zero must_== ""
  }

  "listMonoid" in {
    listMonoid[Int].op(List(1), List(2,3)) must_== List(1,2,3)
    listMonoid[Int].zero must_== List.empty[Int]
  }

  "intAddition" in {
    intAddition.op(1,2) must_== 3
    intAddition.zero must_== 0
    intAddition.op(1, intAddition.zero) must_== 1
  }

  "intMultiplication" in {
    intMultiplication.op(2,3) must_== 6
    intMultiplication.zero must_== 1
    intMultiplication.op(2, intMultiplication.zero) must_== 2
  }

  "booleanOr" in {
    booleanOr.op(true, false) must_== true
    booleanOr.zero must_== false
    booleanOr.op(true, booleanOr.zero) must_== true
    booleanOr.op(false, booleanOr.zero) must_== false
  }

  "booleanAnd" in {
    booleanAnd.op(true, false) must_== false
    booleanAnd.zero must_== true
    booleanAnd.op(true, booleanAnd.zero) must_== true
    booleanAnd.op(false, booleanAnd.zero) must_== false
  }

  "optionMonoid" in {
    optionMonoid.op(Some(1), Some(2)) must_== Some(1)
    optionMonoid.op(None, Some(2)) must_== Some(2)
    optionMonoid.zero must_== None
  }

  "endoMonoid" in {
    endoMonoid[Int].op(_ + 2, _ * 3)(5) must_== 21
    endoMonoid[Int].zero(5) must_== 5
  }
}
