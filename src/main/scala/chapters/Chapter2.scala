package chapters
import scala.annotation.tailrec

object Chapter2 {

  // exercise 2.1
  // this version gives the fib number 0 to index 0, not 1
  // e.g. fib 10 = 55
  def fib(n: Int): Int = {
    @tailrec
    def go(index: Int = 0, lastFib: Int = 0, currentFib: Int = 0): Int = {
      if (index == n) currentFib
      else if (index <= 0) go(index + 1, currentFib, 1)
      else go(index + 1, currentFib, lastFib + currentFib)
    }
    if (n < 0) -1 else go()
  }

  // exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    val size = as.size
    def orderedAt(index: Int) = ordered(as(index), as(index + 1))

    @tailrec
    def go(index: Int = 0): Boolean = {
      if (index + 1 == size) true
      else if (orderedAt(index)) go(index + 1)
      else false
    }

    size == 0 || go()
  }

  // exercise 2.3
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = a => b => f(a,b)

  // exercise 2.4
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = (a,b) => f(a)(b)

  // exercise 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
}
