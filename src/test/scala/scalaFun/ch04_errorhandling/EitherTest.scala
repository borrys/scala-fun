package scalaFun.ch04_errorhandling

import org.scalatest.FreeSpec

class EitherTest extends FreeSpec {
  val left: Either[String, Int] = Left("error")
  val right: Either[String, Int] = Right(42)

  "map" - {
    "when called on Left returns the same Left" in {
      assert(left.map(_ + 1) === left)
    }

    "when called on Right returns Right with mapped value" in {
      assert(right.map(_ + 1) === Right(43))
    }
  }

  "flatMap" - {
    "when called on Left returns the same Left" in {
      assert(left.flatMap(v => Right(v)) === left)
    }

    "when called on Right returns Right obtained from function" in {
      assert(right.flatMap(v => Right(v + 1)) === Right(43))
    }

    "when called on Right returns Left obtained from function" in {
      assert(right.flatMap(v => left) === left)
    }
  }

  "orElse" - {
    "when called on Left returns provided right" in {
      assert(left.orElse(right) === right)
    }

    "when called on Right returns the same right" in {
      assert(right.orElse(Right(999)) === right)
    }
  }

  "map2" - {
    "with two Rights return Right of applied function" in {
      assert(Right(5).map2(Right(7))(_ + _) == Right(12))
    }

    "with Left and Right returns Left" in {
      assert(left.map2(right)(_ + _) == left)
    }

    "with Right and Left returns Left" in {
      assert(right.map2(left)(_ + _) == left)
    }

    "with two Lefts returns first Left" in {
      val first: Either[String, Int] = Left("first")
      val second: Either[String, Int] = Left("second")
      assert(first.map2(second)(_ + _) == first)
    }
  }
}
