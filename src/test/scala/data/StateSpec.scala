package data
import org.specs2.mutable.Specification

class StateSpec extends Specification {

  def state(n: Int) = State.unit[Int,Int](n)

  "unit" in {
    state(5).run(1) must_== (5,1)
  }

  "flatMap" in {
    state(5).flatMap(n => state(n*2)).run(1) must_== (10,1)
  }

  "map" in {
    state(5).map(_*2).run(1) must_== (10,1)
  }

  "map2" in {
    state(5).map2(state(10))((_+_)).run(1) must_== (15,1)
  }

  "sequence" in {
    State.sequence(List(state(5), state(10), state(15))).run(1) must_== (List(5,10,15), 1)
  }
}
