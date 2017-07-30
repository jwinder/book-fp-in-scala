package data

import org.specs2.mutable.Specification
import org.specs2.specification.{Scope, After}
import java.util.concurrent._

class FoldableSpec extends Specification {
  import Monoid._
  import Foldable._

  "Foldable#toList" in {
    foldableList.toList(List(1,2,3)) must_== List(1,2,3)
    foldableIndexedSeq.toList(IndexedSeq(1,2,3)) must_== List(1,2,3)
    foldableStream.toList(Stream(1,2,3)) must_== List(1,2,3)
    foldableTree.toList(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) must_== List(1,2,3)
    foldableOption.toList(Some(1)) must_== List(1)
  }
}
