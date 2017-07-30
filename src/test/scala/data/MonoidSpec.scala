package data

import org.specs2.mutable.Specification
import org.specs2.specification.{Scope, After}
import java.util.concurrent._

class MonoidSpec extends Specification {
  import Monoid._
  import check._

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

  "monoidLaws" in {
    val prop = monoidLaws(intAddition, Gen.choose(0, 10000))
    Prop.run(prop, 10, 10).isPassed must beTrue
  }

  "foldMap" in {
    foldMap(List("2","4","6"), intMultiplication)(_.toInt) must_== 48
  }

  "foldMapV" in {
    foldMapV(IndexedSeq("2","4","6"), intMultiplication)(_.toInt) must_== 48
    foldMapV(IndexedSeq("1"), intMultiplication)(_.toInt) must_== 1
  }

  "par.apply" in new parContext {
    import concurrent._
    import Par._

    val parIntAddition = par(intAddition)
    parIntAddition.zero(es).get must_== 0
    parIntAddition.op(unit(5), unit(10))(es).get must_== 15
  }

  "par.foldMap" in new parContext {
    import concurrent._
    import Par._

    par.foldMap(IndexedSeq("2","4","6"), intMultiplication)(_.toInt)(es).get must_== 48
    par.foldMap(IndexedSeq("1"), intMultiplication)(_.toInt)(es).get must_== 1
  }

  "productMonoid" in {
    val m = productMonoid(intAddition, intMultiplication)
    m.zero must_== (0,1)
    m.op((2,2), (3,3)) must_== (5,6)
  }

  "functionMonoid" in {
    val m = functionMonoid[Int, Int](intMultiplication)
    m.zero(10) must_== 1
    m.op(_*2, _*5)(5) must_== 250
  }

  "mapMergeMonoid" in {
    val m = mapMergeMonoid[Int, Int](intMultiplication)
    m.op(Map(1 -> 2, 2 -> 4, 4 -> 8), Map(2 -> 6, 5 -> 10)) must havePairs(1 -> 2, 2 -> 24, 4 -> 8, 5 -> 10)
  }

  trait parContext extends Scope with After {
    lazy val es = Executors.newFixedThreadPool(25)
    def after() = es.shutdownNow()
  }
}
