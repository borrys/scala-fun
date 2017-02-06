package scalaFun.ch02_gettingstarted

import scala.annotation.tailrec

object Fibonacci {
  def getNth(n: Int): Int = {
    @tailrec
    def loop(a:Int, b: Int, i: Int): Int = {
      if(i==0) a
      else loop(b, a + b, i - 1)
    }
    loop(0, 1, n)
  }

}
