package scalaFun.ch05_laziness

import org.scalatest.FreeSpec

import scalaFun.ch05_laziness.Stream.empty

class StreamTest extends FreeSpec {
  "toList" - {
    "on empty stream returns empty list" in {
      assert(empty.toList == List())
    }
    "on non-empty stream returns list with all values" in {
      assert(Stream(1, 2, 3).toList == List(1, 2, 3))
    }
  }

  "take" - {
    "returns stream of first n elements" in {
      assert(Stream(1, 2, 3, 4, 5).take(3).toList == List(1, 2, 3))
    }
  }

  "drop" - {
    "returns stream with initial n elements omitted" in {
      assert(Stream(1, 2, 3, 4, 5).drop(3).toList == List(4, 5))
    }
  }

  "takeWhile" - {
    "returns stream of initial elements matching predicate" in {
      assert(Stream("bar", "baz", "foo", "baf").takeWhile(_.startsWith("b")).toList == List("bar", "baz"))
    }
  }
}
