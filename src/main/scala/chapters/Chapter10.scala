package chapters

object Chapter10 {
  import data._
  import Monoid._

  // other exercises are done in data/Monoid.scala and data/Foldable.scala

  // exercise 10.9
  case class IsSortedProgress(min: Int, max: Int, sortShortCircuit: Boolean = true) {
    def &&(other: IsSortedProgress) = {
      if (sortShortCircuit && other.sortShortCircuit && min <= other.min && max <= other.max && min <= other.max) {
        IsSortedProgress(min, other.max, true)
      } else {
        IsSortedProgress(-1, -1, false)
      }
    }
    def sorted = min <= max && sortShortCircuit
  }
  object IsSortedProgress {
    def apply(n: Int) = new IsSortedProgress(n,n)
    val monoid = new Monoid[IsSortedProgress] {
      def zero = IsSortedProgress(0)
      def op(p1: IsSortedProgress, p2: IsSortedProgress) = p1 && p2
    }
  }

  def isSorted(ns: IndexedSeq[Int]): Boolean = {
    foldMapV(ns, IsSortedProgress.monoid)(IsSortedProgress.apply).sorted
  }

  // "lorem ipsum dolor sit amet, " -> split in half ->
  //    "lorem ipsum do" + "lor sit amet, "

  // "lorem ipsum do" -> Part("lorm", 1, "do")
  // "lor sit amet, " -> Part("lor", 2, "")

  sealed trait WC {
    def size: Int
  }
  case class Stub(chars: String) extends WC {
    def size: Int = if (chars.trim.isEmpty) 0 else 1
  }
  case class Part(lStub: String, words: Int, rStub: String) extends WC {
    def size: Int = (if (lStub.trim.isEmpty) 0 else 1) + words + (if (rStub.trim.isEmpty) 0 else 1)
  }

  object WC {
    def apply(chars: String): WC = {
      if (chars.trim.isEmpty) Part("", 0, "")
      else Stub(chars)
    }

    // exercise 10.10
    val monoid = new Monoid[WC] {
      def zero: WC = WC("")
      def op(a: WC, b: WC): WC = (a,b) match {
        case (Stub(a), Stub(b)) => Stub(a+b)
        case (Stub(a), Part(b,c,d)) => Part(a+b,c,d)
        case (Part(a,b,c), Stub(d)) => Part(a,b,c+d)
        case (Part(a,b,c), Part(d,e,f)) => {
          val middleSize = if (c + d isEmpty) 0 else 1
          Part(a, b + middleSize + e, f)
        }
      }
    }

    // exercise 10.11
    def count(s: String): Int = foldMapV(s.toIndexedSeq, monoid)(ch => WC(ch.toString)).size
  }

  // exercise 10.18
  // bag(Vector("a", "rose", "is", "a", "rose")) ⟹ Map("a" -> 2, "rose" -> 2, "is" -> 1)
  def bag[A](as: IndexedSeq[A]): Map[A,Int] = {
    foldMapV(as, mapMergeMonoid[A,Int](intAddition))(a => Map(a -> 1))
  }
}
