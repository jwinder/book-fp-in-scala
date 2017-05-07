package data
import org.specs2.mutable.Specification

class ListSpec extends Specification {

  "flatMap" in {
    List.flatMap(List(1,2,3))(n => List(n, 2*n)) must_== List(1, 2, 2, 4, 3, 6)
    List(1,2,3).flatMap(n => List(n, 2*n)) must_== List(1, 2, 2, 4, 3, 6)
  }

  "hasSubsequence" in {
    List.hasSubsequence(Nil, Nil) must beTrue
    List.hasSubsequence(List(1,2,3), Nil) must beTrue
    List.hasSubsequence(Nil, List(1,2,3)) must beFalse
    List.hasSubsequence(List(1,2,3,4), List(1,2,3))  must beTrue
    List.hasSubsequence(List(5,6,1,2,3,4), List(1,2,3)) must beTrue
    List.hasSubsequence(List(5,6,1,2,3,4), List(3,4,5)) must beFalse
    List.hasSubsequence(List(5,6,1,2,3,4), List(1,3)) must beFalse

    Nil.hasSubsequence(Nil) must beTrue
    List(1,2,3).hasSubsequence(Nil) must beTrue
    Nil.hasSubsequence(List(1,2,3)) must beFalse
    List(1,2,3,4).hasSubsequence(List(1,2,3)) must beTrue
    List(5,6,1,2,3,4).hasSubsequence(List(1,2,3)) must beTrue
    List(5,6,1,2,3,4).hasSubsequence(List(3,4,5)) must beFalse
    List(5,6,1,2,3,4).hasSubsequence(List(1,3)) must beFalse
  }

  "fill" in {
    List.fill(5)(1) must_== List(1,1,1,1,1)
  }

  "length" in {
    List.empty[Int].length must_== 0
    List(1).length must_== 1
    List(1,2,3).length must_== 3
  }

  "getAtIndex" in {
    List.empty[Int].getAtIndex(0) must_== Option.none[Int]
    List.empty[Int].getAtIndex(1) must_== Option.none[Int]
    List.empty[Int].getAtIndex(10) must_== Option.none[Int]

    List(1).getAtIndex(0) must_== Option.some(1)
    List(1).getAtIndex(1) must_== Option.none[Int]

    List(1,2,3).getAtIndex(0) must_== Option.some(1)
    List(1,2,3).getAtIndex(1) must_== Option.some(2)
    List(1,2,3).getAtIndex(2) must_== Option.some(3)
    List(1,2,3).getAtIndex(4) must_== Option.none[Int]
  }

  "sorted" in {
    List.empty[Int].sorted must_== List.empty
    List(1).sorted must_== List(1)
    List(1,2).sorted must_== List(1,2)
    List(2,1).sorted must_== List(1,2)
    List(1,2,3).sorted must_== List(1,2,3)
    List(3,2,1).sorted must_== List(1,2,3)
    List(2,3,1).sorted must_== List(1,2,3)
    List(2,2,2,3,1).sorted must_== List(1,2,2,2,3)
    List(100 to 1 by -1: _*).sorted must_== List(1 to 100: _*)
  }

  "forall" in {
    List(2,4).forall(_ % 2 == 0) must beTrue
    List(1,2,4).forall(_ % 2 == 0) must beFalse
    List.empty[Int].forall(_ % 2 == 0) must beTrue
  }

  "foreach" in {
    var sum = 0
    List(1,2,3).foreach(sum += _)
    sum must_== 6
  }

  "head/headOption" in {
    List(1,2,3).head must_== 1
    List.empty.head must throwA[IndexOutOfBoundsException]
    List(1,2,3).headOption must_== Option.some(1)
    List.empty.headOption must_== None
  }

  "last/lastOption" in {
    List(1,2,3).last must_== 3
    List.empty.last must throwA[IndexOutOfBoundsException]
    List(1,2,3).lastOption must_== Option.some(3)
    List.empty.lastOption must_== None
  }

  "min/max/minOption/maxOption" in {
    List(1,2,3).minOption must_== Option.some(1)
    List(1,2,3).maxOption must_== Option.some(3)
    List.empty[Int].minOption must_== Option.none
    List.empty[Int].maxOption must_== Option.none

    List(1,2,3).min must_== 1
    List(1,2,3).max must_== 3
  }

  "exists" in {
    List(1,2,3).exists(_ == 2) must beTrue
    List(1,2,3).exists(_ == 5) must beFalse
  }

  "contains" in {
    List(1,2,3).contains(2) must beTrue
    List(1,2,3).contains(5) must beFalse
  }
}
