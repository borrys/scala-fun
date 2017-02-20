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

  "forAll" - {
    "returns true if all elements in stream matches predicate" in {
      assert(Stream(1, 2, 3, 4, 5).forAll(_ < 100) == true)
    }

    "returns false if at least one element does not match predicate" in {
      assert(Stream(1, 2, 3, 4, 5).forAll(_ != 3) == false)
    }
  }

  "headOption" - {
    "returns None for empty stream" in {
      assert(empty.headOption.isEmpty)
    }

    "returns Some of first element for non-empty stream" in {
      assert(Stream(1, 2, 3, 4).headOption.contains(1))
    }
  }

  "map" - {
    "applies function to all arguments" in {
      assert(Stream(1, 2, 3).map(_ + 1).toList == List(2, 3, 4))
    }
  }

  "filter" - {
    "skips all elements not matching predicate" in {
      assert(Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList == List(2, 4))
    }
  }

  "append" - {
    "adds all elements to the end of stream" in {
      assert(Stream(1, 2, 3).append(Stream(4, 5, 6)).toList == List(1, 2, 3, 4, 5, 6))
    }
  }

  "flatMap" - {
    "returns stream of flattened function results" in {
      assert(Stream(1, 2, 3).flatMap(x => Stream(x, x)).toList == List(1, 1, 2, 2, 3, 3))
    }
  }

  "unfold" - {
    "returns infinite stream of generated values" in {
      assert(Stream.unfold(0)(x => Some(x, x + 1)).take(4).toList == List(0, 1, 2, 3))
    }

    "returns finite stream of generated values" in {
      assert(Stream.unfold(10)(x => if (x > 5) Some(x, x - 1) else None).toList == List(10, 9, 8, 7, 6))
    }

    "returns empty stream" in {
      assert(Stream.unfold("initial state")(_ => None).toList == List())
    }
  }

  "fibs" - {
    "returns fibonacci sequence" in {
      assert(Stream.fibs.take(6).toList == List(1, 1, 2, 3, 5, 8))
    }
  }

  "startsWith" - {
    "determines that stream starts with another" in {
      assert(Stream(1, 2, 3).startsWith(Stream(1, 2)))
    }

    "determines that stream does not start with another" in {
      assert(!Stream(1, 2, 3).startsWith(Stream(5, 6)))
    }
  }
}
