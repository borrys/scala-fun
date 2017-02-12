package scalaFun.ch03_datastructures

import org.scalatest.FreeSpec

import scalaFun.ch03_datastructures.Tree._

class Tree$Test extends FreeSpec {
  "Tree.size" - {
    "is 1 for single leaf" in {
      assert(size(Leaf("test")) == 1)
    }

    "is 3 for branch with two leafs" in {
      assert(size(Branch(Leaf("left"), Leaf("right"))) == 3)
    }

    "is 5 for tree with 5 nodes" in {
      assert(size(Branch(Leaf("left"), Branch(Leaf("right_1"), Leaf("right_2")))) == 5)
    }
  }

  "Tree.maximum" - {
    "is leaf value for single leaf" in {
      assert(maximum(Leaf(100)) == 100)
    }

    "is the grater of leaf values under branch" in {
      assert(maximum(Branch(Leaf(5), Leaf(8))) == 8)
    }

    "is the gratest value in whole tree" in {
      assert(maximum(Branch(Branch(Leaf(6), Leaf(42)), Leaf(12))) == 42)
    }
  }

  "Tree.depth" - {
    "is 1 for single leaf" in {
      assert(depth(Leaf("single leaf")) == 1)
    }

    "is 2 for branch with two leaves" in {
      assert(depth(Branch(Leaf(42), Leaf(84))) == 2)
    }

    "is 4 for tree with 4 levels" in {
      val tree = Branch(
        Leaf("left"),
        Branch(
          Leaf("right_1"),
          Branch(
            Leaf("right_2_1"),
            Leaf("right_2_2")))
      )
      assert(depth(tree) == 4)
    }
  }

  "Tree.map" - {
    "applies function to leaf value" in {
      assert(map(Leaf(5))(_ * 10) == Leaf(50))
    }

    "converts each value in a tree with function" in {
      val input = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
      val expected = Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))
      assert(map(input)(_ + 1) == expected)
    }
  }
}
