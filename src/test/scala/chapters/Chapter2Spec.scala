package chapters
import org.specs2.mutable.Specification

class Chapter2Spec extends Specification {
  import Chapter2._

  "fib" in {
    fib(10) must_== 55
  }

  "isSorted" in {
    val `<` = Ordering.Int.lt(_,_)
    isSorted(Array(1,2,3,4), `<`) must beTrue
    isSorted(Array(1,3,2,4), `<`) must beFalse
    isSorted(Array(), `<`) must beTrue
  }
}
