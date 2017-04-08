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
}
