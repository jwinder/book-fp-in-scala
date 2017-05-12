package chapters

import org.specs2.mutable.Specification

class Chapter6Spec extends Specification {
  import Chapter6._
  import data._

  "candy machine - inserting a coin into a locked machine causes an unlock if there is candy left" in {
    Candy.Machine(true, 10, 10).input(Candy.Coin) must_== Candy.Machine(false, 10, 11)
    Candy.Machine(true, 0, 10).input(Candy.Coin) must_== Candy.Machine(true, 0, 10)
  }

  "candy machine - a turn on an unlocked machine causes candy to dispense and become locked" in {
    Candy.Machine(false, 10, 10).input(Candy.Turn) must_== Candy.Machine(true, 9, 10)
    Candy.Machine(true, 10, 10).input(Candy.Turn) must_== Candy.Machine(true, 10, 10)
  }

  "candy machine - ignore all inputs when in the wrong states" in {
    Candy.Machine(true, 10, 10).input(Candy.Turn) must_== Candy.Machine(true, 10, 10)
    Candy.Machine(false, 10, 10).input(Candy.Coin) must_== Candy.Machine(false, 10, 10)
    Candy.Machine(true, 0, 10).input(Candy.Turn) must_== Candy.Machine(true, 0, 10)
    Candy.Machine(true, 0, 10).input(Candy.Coin) must_== Candy.Machine(true, 0, 10)
  }

  "candy machine - simulateMachine" in {
    val inputs = List(Candy.Coin, Candy.Turn, Candy.Coin, Candy.Turn)
    Candy.simulateMachine(inputs).run(Candy.Machine(true, 5, 10)) must_== ((12, 3), Candy.Machine(true, 3, 12))
  }
}
