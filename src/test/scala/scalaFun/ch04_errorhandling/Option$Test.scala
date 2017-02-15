package scalaFun.ch04_errorhandling

import org.scalatest.FreeSpec

import scalaFun.ch04_errorhandling.Option.{sequence, traverse, variance}

class Option$Test extends FreeSpec {
  "variance" - {
    "returns None for empty input list" in {
      assert(variance(List()) == None)
    }

    "returns Some(0) for single element list" in {
      assert(variance(List(20.0)) == Some(0.0))
    }

    "returns non-zero variance list of different values" in {
      assert(variance(List(1.0, 2.0, 3.0, 4.0)) == Some(1.25))
    }

    "returns zero variance list of equal values" in {
      assert(variance(List(42.0, 42.0, 42.0, 42.0)) == Some(0.0))
    }
  }

  "sequence" - {
    "returns Some with list of all internal values" - {
      assert(sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
    }

    "returns None for list with at least one None" - {
      assert(sequence(List(Some(1), None, Some(3))) == None)
    }
  }

  "traverse" - {
    "returns Some of list containing values transformed with given function" in {
      assert(traverse(List(1, 2, 3))(i => Some(i * 2)) == Some(List(2, 4, 6)))
    }

    "returns None when function returns None for at least one of elements in original list" in {
      assert(traverse(List(1, 2, 3))(i => if (i == 2) None else Some(i)) == None)
    }

  }
}
