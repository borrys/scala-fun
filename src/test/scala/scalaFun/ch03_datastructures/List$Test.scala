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
}