package data
import org.specs2.mutable.Specification

class StreamSpec extends Specification {

  "equiv" in {
    Stream(1,2,3).equiv(Stream(1,2,3)) must beTrue
    Stream(1,2,3).equiv(Stream(3,2,1)) must beFalse
    Stream.empty.equiv(Stream.empty) must beTrue
    Stream(1,2,3).equiv(Stream.empty) must beFalse
  }

  "headOption" in {
    Stream(1,2,3,4).headOption must_== Some(1)
    Stream.empty.headOption must_== None

    Stream(1,2,3,4).headOption2 must_== Some(1)
    Stream.empty.headOption2 must_== None
  }

  "headOption" in {
    Stream(1,2,3,4).lastOption must_== Some(4)
    Stream.empty.lastOption must_== None
  }

  "toList" in {
    Stream(1,2,3,4).toList must_== List(1,2,3,4)
    Stream.empty.toList must_== List.empty
  }

  "take" in {
    Stream(1,2,3).take(1).equiv(Stream(1)) must beTrue
    Stream(1,2,3).take(0).equiv(Stream.empty) must beTrue
    Stream(1,2,3).take(5).equiv(Stream(1,2,3)) must beTrue
    Stream.empty.take(1).equiv(Stream.empty) must beTrue

    Stream(1,2,3).take2(1).equiv(Stream(1)) must beTrue
    Stream(1,2,3).take2(0).equiv(Stream.empty) must beTrue
    Stream(1,2,3).take2(5).equiv(Stream(1,2,3)) must beTrue
    Stream.empty.take2(1).equiv(Stream.empty) must beTrue
  }

  "drop" in {
    Stream(1,2,3,4).drop(1).equiv(Stream(2,3,4)) must beTrue
    Stream(1,2,3,4).drop(0).equiv(Stream(1,2,3,4)) must beTrue
    Stream(1,2,3,4).drop(5).equiv(Stream.empty) must beTrue
    Stream(1).drop(2).equiv(Stream.empty) must beTrue
    Stream.empty.drop(1).equiv(Stream.empty) must beTrue
  }

  "takeWhile" in {
    Stream(1,2,3,4,5,6).takeWhile(_ < 4).equiv(Stream(1,2,3)) must beTrue
    Stream(1,2,3,4,5,6).takeWhile(_ < 1).equiv(Stream.empty) must beTrue
    Stream.empty[Int].takeWhile(_ < 10).equiv(Stream.empty) must beTrue

    Stream(1,2,3,4,5,6).takeWhile2(_ < 4).equiv(Stream(1,2,3)) must beTrue
    Stream(1,2,3,4,5,6).takeWhile2(_ < 1).equiv(Stream.empty) must beTrue
    Stream.empty[Int].takeWhile2(_ < 10).equiv(Stream.empty) must beTrue

    Stream(1,2,3,4,5,6).takeWhile3(_ < 4).equiv(Stream(1,2,3)) must beTrue
    Stream(1,2,3,4,5,6).takeWhile3(_ < 1).equiv(Stream.empty) must beTrue
    Stream.empty[Int].takeWhile3(_ < 10).equiv(Stream.empty) must beTrue
  }

  "exists" in {
    Stream(1,2,3,4,5,6).exists(_ == 3) must beTrue
    Stream(1,2,3,4,5,6).exists(_ == 10) must beFalse
  }

  "forAll" in {
    Stream(1,2,3,4,5).forAll(_ < 10) must beTrue
    Stream(1,2,3,4,5).forAll(_ <= 5) must beTrue
    Stream(1,2,3,4,5).forAll(_ < 3) must beFalse
    Stream.empty[Int].forAll(_ < 10) must beTrue
  }

  "map" in {
    Stream(1,2,3).map(_ * 2).equiv(Stream(2,4,6)) must beTrue
    Stream.empty[Int].map(_ * 2).equiv(Stream.empty) must beTrue

    Stream(1,2,3).map2(_ * 2).equiv(Stream(2,4,6)) must beTrue
    Stream.empty[Int].map2(_ * 2).equiv(Stream.empty) must beTrue
  }

  "filter" in {
    Stream(1,2,3,4,5,6).filter(_ % 2 == 0).equiv(Stream(2,4,6)) must beTrue
    Stream.empty[Int].filter(_ % 2 == 0).equiv(Stream.empty) must beTrue
  }

  "append" in {
    Stream(1,2,3,4).append(Stream(5,6,7)).equiv(Stream(1,2,3,4,5,6,7)) must beTrue
    Stream.empty[Int].append(Stream(5,6,7)).equiv(Stream(5,6,7)) must beTrue
    Stream(1,2,3).append(Stream.empty).equiv(Stream(1,2,3)) must beTrue
    Stream.empty[Int].append(Stream.empty[Int]).equiv(Stream.empty) must beTrue
  }

  "flatMap" in {
    Stream(1,2,3,4).flatMap(i => Stream(i, 10*i)).equiv(Stream(1,10,2,20,3,30,4,40)) must beTrue
    Stream.empty[Int].flatMap(i => Stream(i, 10*i)).equiv(Stream.empty) must beTrue
  }

  "find" in {
    Stream(1,2,3,4).find(_ == 3) must_== Option.some(3)
    Stream(1,2,3,4).find(_ == 10) must_== Option.none
    Stream.empty[Int].find(_ == 10) must_== Option.none
  }

  "ones" in {
    Stream.ones.take(5).equiv(Stream(1,1,1,1,1)) must beTrue
    Stream.ones2.take(5).equiv(Stream(1,1,1,1,1)) must beTrue
  }

  "fiveOnes" in {
    Stream.fiveOnes() must_== List(1,1,1,1,1)
  }

  "constant" in {
    Stream.constant(5).take(5).equiv(Stream(5,5,5,5,5)) must beTrue
    Stream.constant(5).take(0).equiv(Stream.empty) must beTrue

    Stream.constant2(5).take(5).equiv(Stream(5,5,5,5,5)) must beTrue
    Stream.constant2(5).take(0).equiv(Stream.empty) must beTrue
  }

  "from" in {
    Stream.from(5).take(5).equiv(Stream(5,6,7,8,9)) must beTrue
    Stream.from(-5).take(5).equiv(Stream(-5,-4,-3,-2,-1)) must beTrue

    Stream.from2(5).take(5).equiv(Stream(5,6,7,8,9)) must beTrue
    Stream.from2(-5).take(5).equiv(Stream(-5,-4,-3,-2,-1)) must beTrue
  }

  "zipWith" in {
    Stream(1,2,3,4).zipWith(Stream(2,3,4,5))(_ + _).equiv(Stream(3,5,7,9)) must beTrue
    Stream(1,2,3,4).zipWith2(Stream(2,3,4,5))(_ + _).equiv(Stream(3,5,7,9)) must beTrue
  }

  "fibs" in {
    Stream.fibs.take(8).equiv(Stream(0,1,1,2,3,5,8,13)) must beTrue
    Stream.fibs2.take(8).equiv(Stream(0,1,1,2,3,5,8,13)) must beTrue
  }

  "unfold" in {
    Stream.unfold(1)(n => Some(n,n+1)).take(5).equiv(Stream(1,2,3,4,5)) must beTrue
    Stream.unfold(1)(n => None).equiv(Stream.empty) must beTrue
  }

  "zipAll" in {
    Stream(1,2,3).zipAll(Stream(5,6,7)).equiv(Stream((Some(1),Some(5)), (Some(2),Some(6)), (Some(3),Some(7)))) must beTrue
    Stream(1,2,3).zipAll(Stream(5)).equiv(Stream((Some(1),Some(5)), (Some(2),None), (Some(3),None))) must beTrue
    Stream(1).zipAll(Stream(5,6,7)).equiv(Stream((Some(1),Some(5)), (None,Some(6)), (None,Some(7)))) must beTrue
    Stream(1,2,3).zipAll(Stream.empty).equiv(Stream((Some(1),None), (Some(2),None), (Some(3),None))) must beTrue
    Stream.empty.zipAll(Stream(5,6,7)).equiv(Stream((None,Some(5)), (None,Some(6)), (None,Some(7)))) must beTrue
    Stream.empty[Int].zipAll(Stream.empty).equiv(Stream.empty) must beTrue
  }

  "startsWith" in {
    Stream(1,2,3).startsWith(Stream(1,2)) must beTrue
    Stream(1,2,3).startsWith(Stream(1,2,3,4)) must beFalse
    Stream(1,2,3).startsWith(Stream(2,3)) must beFalse
    Stream(1,2,3).startsWith(Stream.empty) must beTrue
    Stream.empty.startsWith(Stream(1,2)) must beFalse
    Stream.empty.startsWith(Stream.empty) must beTrue
  }

  "tails" in {
    Stream(1,2,3).tails.map(_.toList).toList must_== List(List(1,2,3), List(2,3), List(3), List.empty)
    Stream(1).tails.map(_.toList).toList must_== List(List(1), List.empty)
    Stream.empty[Int].tails.map(_.toList).toList must_== List(List.empty)
  }

  "hasSubsequence" in {
    Stream.empty.hasSubsequence(Stream.empty) must beTrue
    Stream(1,2,3).hasSubsequence(Stream.empty) must beTrue
    Stream.empty.hasSubsequence(Stream(1,2,3)) must beFalse
    Stream(1,2,3,4).hasSubsequence(Stream(1,2,3)) must beTrue
    Stream(5,6,1,2,3,4).hasSubsequence(Stream(1,2,3)) must beTrue
    Stream(5,6,1,2,3,4).hasSubsequence(Stream(3,4,5)) must beFalse
    Stream(5,6,1,2,3,4).hasSubsequence(Stream(1,3)) must beFalse
  }

  "scanRight" in {
    Stream(1,2,3).scanRight(0)(_ + _).equiv(Stream(6,5,3,0)) must beTrue
    Stream.empty[Int].scanRight(0)(_ + _).equiv(Stream(0)) must beTrue

    Stream(1,2,3).scanRight2(0)(_ + _).equiv(Stream(6,5,3,0)) must beTrue
    Stream.empty[Int].scanRight2(0)(_ + _).equiv(Stream(0)) must beTrue
  }
}
