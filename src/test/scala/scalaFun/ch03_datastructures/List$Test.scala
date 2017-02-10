package scalaFun.ch03_datastructures

import org.scalatest.WordSpec

import scalaFun.ch03_datastructures.List._

class List$Test extends WordSpec {
  "List.tail" when {
    "given empty list" should {
      "return empty list" in {
        assert(tail(List()) == List())
      }
    }
    "given non-empty list" should {
      "return all elements of the list but first" in {
        assert(tail(List(1, 2, 3, 4)) == List(2, 3, 4))
      }
    }
  }

  "List.setHead" when {
    "given empty list" should {
      "raise an exception for empty list" in {
        assertThrows[RuntimeException] {
          setHead(List(), 5)
        }
      }
    }

    "given non-empty list" should {
      "return list with first element swapped" in {
        assert(setHead(List("a", "b", "c"), "zzz") == List("zzz", "b", "c"))
      }
    }
  }


  "List.drop" when {
    "given empty list" should {
      "raise an exception for empty list" in {
        assertThrows[RuntimeException] {
          drop(List(), 1)
        }
      }
    }

    "given non-empty list and number smaller than list length" should {
      "return list with removed initial elements" in {
        assert(drop(List("a", "b", "c", "d"), 2) == List("c", "d"))
      }
    }

    "given non-empty list and negative number" should {
      "return the whole list" in {
        assert(drop(List(1, 2, 3, 4), -5) == List(1, 2, 3, 4))
      }
    }

    "given empty list and number greater than list length" should {
      "raise an exception" in {
        assertThrows[RuntimeException] {
          drop(List(1, 2, 3), 150)
        }
      }
    }
  }

  "List.dropWhile" when {
    "given empty list" should {
      "return empty list" in {
        assert(dropWhile(List[Int]())(_ > 5) == List())
      }
    }

    "given non-empty list and condition not fulfilled by any element" should {
      "return the whole list" in {
        assert(dropWhile(List(1, 2, 3, 4))(_ > 100) == List(1, 2, 3, 4))
      }
    }

    "given non-empty list and condition fulfilled by initial elements" should {
      "return list without those initial elements" in {
        assert(dropWhile(List(1, 2, 3, 4))(_ < 3) == List(3, 4))
      }
    }

    "given non-empty list and condition fulfilled by elements in the middle" should {
      "return the whole list" in {
        assert(dropWhile(List(1, 200, 300, 4))(_ > 100) == List(1, 200, 300, 4))
      }
    }
  }

  "List.init" when {
    "given empty list" should {
      "throw exception" in {
        assertThrows[RuntimeException] {
          init(List())
        }
      }
    }

    "given list with single element" should {
      "return empty list" in {
        assert(init(List("a")) == List())
      }
    }

    "given list with many elements" should {
      "return list of all elements but last" in {
        assert(init(List(1, 2, 3, 4, 5, 6)) == List(1, 2, 3, 4, 5))
      }
    }
  }

  "List.length" when {
    "given empty list" should {
      "return zero" in {
        assert(length(List()) == 0)
      }
    }

    "given list with five elements" should {
      "return five" in {
        assert(length(List(1, 2, 3, 4, 5)) == 5)
      }
    }
  }

  "List.foldLeft" when {
    "given empty list" should {
      "return initial value" in {
        assert(foldLeft(List(), "")((b, a) => b.concat(a.toString)) == "")
      }
    }
    "given non-empty list" should {
      "return result of folding values from left to right" in {
        assert(foldLeft(List(1, 2, 3), "")((b, a) => b.concat(a.toString)) == "123")
      }
    }
  }

  "List.sumLeft" when {
    "given list of numbers" should {
      "return sum of those numbers" in {
        assert(sumLeft(List(100, 20, 3)) == 123)
      }
    }
  }

  "List.productLeft" when {
    "given list of numbers" should {
      "return product of those numbers" in {
        assert(productLeft(List(5, 6, 7)) == 210)
      }
    }
  }

  "List.reverse" when {
    "given list of strings" should {
      "return list of the same strings in reverse order" in {
        assert(reverse(List("a", "b", "c")) == List("c", "b", "a"))
      }
    }
  }

  "List.append" when {
    "given empty list" should {
      "return list with only appended element" in {
        assert(append(List[Int](), 42) == List(42))
      }
    }

    "given non-empty list" should {
      "return list with new element at the end" in {
        assert(append(List(1, 2, 3), 42) == List(1, 2, 3, 42))
      }
    }
  }

  "List.concatAll" when {
    "given empty list" should {
      "return empty list" in {
        assert(concatAll(List()) == List())
      }
    }
    "given list with one sublist" should {
      "return this list" in {
        assert(concatAll(List(List(1, 2, 3))) == List(1, 2, 3))
      }
    }

    "given list with multiple sublists" should {
      "return list containing elements of all sublists in order" in {
        assert(concatAll(List(List("a"), List("b", "c"), List("d"))) == List("a", "b", "c", "d"))
      }
    }
  }
}