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

  "has map" in {
    s(frank).map(_.name) must_== s("Frank")
    nobody.map(_.name) must_== n
  }

  "has flatMap" in {
    s(frank).flatMap(e => s(e.name)) must_== s("Frank")
    nobody.flatMap(e => s(e.name)) must_== n
  }

  "has getOrElse" in {
    s(frank).getOrElse(bob) must_== frank
    nobody.getOrElse(bob) must_== bob
  }

  "has orElse" in {
    s(frank).orElse(s(bob)) must_== s(frank)
    nobody.orElse(s(bob)) must_== s(bob)
  }

  "has filter" in {
    s(frank).filter(e => e.name == "Frank") must_== s(frank)
    s(frank).filter(e => e.name == "Bob") must_== n

    nobody.filter(e => e.name == "Frank") must_== n
  }
}
