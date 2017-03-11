package example.data
import org.specs2.mutable.Specification

class ListSpec extends Specification {

  "flatMap" in {
    List.flatMap(List(1,2,3))(n => List(n, 2*n)) must_== List(1, 2, 2, 4, 3, 6)
  }

  "hasSubsequence" in {
    List.hasSubsequence(Nil, Nil) must beTrue
    List.hasSubsequence(List(1,2,3), Nil) must beTrue
    List.hasSubsequence(Nil, List(1,2,3)) must beFalse
    List.hasSubsequence(List(1,2,3,4), List(1,2,3))  must beTrue
    List.hasSubsequence(List(5,6,1,2,3,4), List(1,2,3)) must beTrue
    List.hasSubsequence(List(5,6,1,2,3,4), List(3,4,5)) must beFalse
  }
}