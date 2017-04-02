package data
import org.specs2.mutable.Specification

class EitherSpec extends Specification {

  def l[E,A](value: E): Either[E,A] = Either.left(value)
  def r[E,A](value: A): Either[E,A] = Either.right(value)

  "map" in {
    r[Int, Int](5).map(_*10) must_== r(50)
    l[Int, Int](5).map(_*10) must_== l(5)
  }

  "flatMap" in {
    r[Int, Int](5).flatMap(n => r(n*10)) must_== r(50)
    l[Int, Int](5).flatMap(n => r(n*10)) must_== l(5)
    r[Int, Int](5).flatMap(n => l(n*10)) must_== l(50)
  }

  "orElse" in {
    r(5).orElse(r(10)) must_== r(5)
    l(5).orElse(r(10)) must_== r(10)
    l(5).orElse(l(10)) must_== l(10)
  }

  "map2" in {
    def multiply(a: Int, b: Int) = a*b // helps align types
    r(5).map2(r(10))(multiply) must_== r(50)
    l(5).map2(r(10))(multiply) must_== l(5)
    r(5).map2(l(10))(multiply) must_== l(10)
  }

  "traverse" in {
    Either.traverse(List(1,2,3))(i => r(2*i)) must_== r(List(2,4,6))
    Either.traverse(List.empty[Int])(i => r(2*i)) must_== r(List.empty)
    Either.traverse(List(1,2,3))(i => l(i)) must_== l(1)
    Either.traverse(List(1,2,3))(i => if (i == 1) l(i) else r(i)) must_== l(1)
  }

  "sequence" in {
    Either.sequence(List(r(1), r(2), r(3))) must_== r(List(1,2,3))
    Either.sequence(List.empty) must_== r(List.empty)
    Either.sequence(List(l(1), r(2))) must_== l(1)
    Either.sequence(List(r(1), l(2))) must_== l(2)
  }
}
