package scalaFun.ch02_gettingstarted

import org.scalatest.FlatSpec

import scalaFun.ch02_gettingstarted.Order.isSorted

class Order$Test extends FlatSpec {

  "Order" should "mark empty array as sorted" in {
    val emptyArray = Array[Int]()
    assert(isSorted[Int](emptyArray, _ > _))
  }

  it should "mark unordered array as not sorted" in {
    val array = Array[Int](3, 1, 5)
    assert(!isSorted[Int](array, _ > _))
  }

  it should "mark ordered array as sorted" in {
    val array = Array[String]("a", "b", "c")
    assert(isSorted[String](array, (a, b) => a.compareTo(b) <= 0))
  }
}
