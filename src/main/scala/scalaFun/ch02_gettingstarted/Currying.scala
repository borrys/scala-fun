package scalaFun.ch02_gettingstarted

object Currying {

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](g: B => C, f: A => B): A => C =
    a => g(f(a))
}
