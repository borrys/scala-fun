package scalaFun.ch04_errorhandling

import org.scalatest.FreeSpec

class OptionTest extends FreeSpec {
  "map" - {
    "on None returns None" in {
      assert(None.map(_.toString) == None)
    }

    "on Some returns Some with value transformed with given function" in {
      assert(Some(41).map(_ + 1) == Some(42))
    }
  }

  "getOrElse" - {
    "on None returns provided value" in {
      assert(None.getOrElse(42) == 42)
    }

    "on Some returns internal value" in {
      assert(Some(25).getOrElse(999) == 25)
    }
  }

  "flatMap" - {
    "on None returns None" in {
      assert(None.flatMap(v => Some(v.toString)) == None)
    }

    "on Some returns Some returned by function" in {
      assert(Some(5).flatMap(v => Some(v * v)) == Some(25))
    }
  }

  "orElse" - {
    "on None returns provided value" in {
      assert(None.orElse(Some(123)) == Some(123))
    }

    "on Some returns original Option" in {
      assert(Some(99).orElse(Some(-1)) == Some(99))
    }
  }

  "filter" - {
    "on None returns None" in {
      assert((None: Option[Int]).filter(_ > 100) == None)
    }

    "on Some with value satisfying filter returns original Some" in {
      assert(Some(5).filter(_ < 100) == Some(5))
    }

    "on Some with value not satisfying filter returns None" in {
      assert(Some(5).filter(_ < 100) == Some(5))
    }
  }
}
