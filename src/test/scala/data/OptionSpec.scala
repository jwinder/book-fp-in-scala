package data
import org.specs2.mutable.Specification

class OptionSpec extends Specification {

  // helpers to avoid conflicting things in specs2
  def s[A](a: A) = Option.some(a)
  def n[A] = Option.none[A]

  case class Employee(name: String, department: String)

  val frank = Employee("Frank", "Tacos")
  val bob = Employee("Bob", "Trucks")
  val nobody = n[Employee]

  "isSome & isNone" in {
    s(frank).isSome must beTrue
    s(frank).isNone must beFalse

    n.isSome must beFalse
    n.isNone must beTrue
  }

  "map" in {
    s(frank).map(_.name) must_== s("Frank")
    nobody.map(_.name) must_== n
  }

  "flatMap" in {
    s(frank).flatMap(e => s(e.name)) must_== s("Frank")
    nobody.flatMap(e => s(e.name)) must_== n
  }

  "getOrElse" in {
    s(frank).getOrElse(bob) must_== frank
    nobody.getOrElse(bob) must_== bob
  }

  "orElse" in {
    s(frank).orElse(s(bob)) must_== s(frank)
    nobody.orElse(s(bob)) must_== s(bob)
  }

  "filter" in {
    s(frank).filter(e => e.name == "Frank") must_== s(frank)
    s(frank).filter(e => e.name == "Bob") must_== n

    nobody.filter(e => e.name == "Frank") must_== n
  }

  "map2" in {
    val five = s[Int](5)
    val two = s[Int](2)
    val missing = n[Int]

    Option.map2(five, two)(_*_) must_== s(10)
    Option.map2(missing, two)(_*_) must_== n
    Option.map2(five, missing)(_*_) must_== n
  }

  "sequence" in {
    Option.sequence(List(s(5), s(10), s(15))) must_== s(List(5, 10, 15))
    Option.sequence(List.empty) must_== s(List.empty)
    Option.sequence(List(n, s(10), s(15))) must_== n
    Option.sequence(List(s(5), n)) must_== n
  }

  "traverse" in {
    Option.traverse(List(1,2,3))(i => s(2*i)) must_== s(List(2,4,6))
    Option.traverse(List.empty[Int])(i => s(2*i)) must_== s(List.empty)
    Option.traverse(List(1,2,3))(i => n) must_== n
    Option.traverse(List(1,2,3))(i => if (i == 1) n else s(2*i)) must_== n
  }
}
