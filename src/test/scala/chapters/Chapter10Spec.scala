package chapters

import org.specs2.mutable.Specification

class Chapter10Spec extends Specification {
  import Chapter10._

  "isSorted" in {
    isSorted(IndexedSeq.empty[Int]) must beTrue
    isSorted(IndexedSeq(1,2,3)) must beTrue
    isSorted(IndexedSeq(1)) must beTrue
    isSorted(IndexedSeq(1,3,2)) must beFalse
    isSorted(IndexedSeq(3,1,2)) must beFalse
    isSorted(IndexedSeq(3,2,1)) must beFalse
  }

  "WC" in {
    WC.count("") must_== 0
    WC.count("one") must_== 1
    WC.count("one two") must_== 2
    WC.count(" one two   ") must_== 2
    WC.count(" one two, three, four   ") must_== 4
  }

  "bag" in {
    bag(IndexedSeq(1,1,2,2,2,3,3,3,3)) must havePairs(1 -> 2, 2 -> 3, 3 -> 4)
  }
}
