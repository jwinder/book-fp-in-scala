package data
import org.specs2.mutable.Specification

class TreeSpec extends Specification {

  "size2" in {
    Tree.size2(Leaf(1)) must_== 1
    Tree.size2(Branch(Leaf(1), Branch(Leaf(1), Leaf(2)))) must_== 5
  }

  "maximum2" in {
    Tree.maximum2(Leaf(1)) must_== 1
    Tree.maximum2(Branch(Leaf(1), Branch(Leaf(1), Leaf(10)))) must_== 10
  }

  "depth2" in {
    Tree.depth2(Leaf(1)) must_== 1
    Tree.depth2(Branch(Leaf(1), Branch(Leaf(1), Leaf(2)))) must_== 3
  }

  "map2" in {
    Tree.map2(Branch(Leaf(1), Branch(Leaf(1), Leaf(2))))(_ * 2) must_== Branch(Leaf(2), Branch(Leaf(2), Leaf(4)))
  }

  "size" in {
    Tree.size(Leaf(1)) must_== 1
    Tree.size(Branch(Leaf(1), Branch(Leaf(1), Leaf(2)))) must_== 5
  }

  "maximum" in {
    Tree.maximum(Leaf(1)) must_== 1
    Tree.maximum(Branch(Leaf(1), Branch(Leaf(1), Leaf(10)))) must_== 10
  }

  "depth" in {
    Tree.depth(Leaf(1)) must_== 1
    Tree.depth(Branch(Leaf(1), Branch(Leaf(1), Leaf(2)))) must_== 3
  }

  "map" in {
    Tree.map(Branch(Leaf(1), Branch(Leaf(1), Leaf(2))))(_ * 2) must_== Branch(Leaf(2), Branch(Leaf(2), Leaf(4)))
  }
}
