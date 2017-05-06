package chapters
import data._

object Chapter6 {

  // exercise 6.11
  object Candy {
    sealed trait Input
    case object Coin extends Input
    case object Turn extends Input

    case class Machine(locked: Boolean, candies: Int, coins: Int) {
      def input(input: Input): Machine = {
        if (input == Coin && locked && candies > 0) copy(locked = false, coins = coins + 1)
        else if (input == Turn && !locked) copy(locked = true, candies = candies - 1)
        else this
      }
    }

    // operate the machine based on list of inputs
    // returns the number of coins and candies left in machine at the end
    // eg. if the machine has 10 coins & 5 candies and a total of 4 candies are bought, the output should be (14, 1)
    def simulateMachine(inputs: List[Input]): State[Machine, (Int,Int)] = State { machine =>
      val updatedMachine = inputs.foldLeft(machine) { (machine, input) => machine.input(input) }
      ((updatedMachine.coins, updatedMachine.candies), updatedMachine)
    }
  }
}
