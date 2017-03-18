package chapters
import data.Option

object Chapter4 {
  import Option._

  // Option functions are implemented in the data.Option type

  // example of exceptions

  def failingFn(i: Int): Int = {
    // this throws an Exception, and is not referentially transparent
    // see failingFn2
    val y: Int = throw new Exception("fail")
    try {
      val x = 42 + 5
      x + y
    } catch {
      case e: Exception => 43
    }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail")): Int)
    } catch {
      case e: Exception => 43
    }
  }

  // example of using an Option instead of throwing exceptions

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) none
    else some(xs.sum / xs.length)
  }

  // exercise 4.2, implement variance using flatMap
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap { xsMean =>
      mean {
        xs.map { x => math.pow(x - xsMean, 2) }
      }
    }
  }
}
