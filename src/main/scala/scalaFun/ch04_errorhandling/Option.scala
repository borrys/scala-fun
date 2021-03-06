package scalaFun.ch04_errorhandling

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(a => Some(a)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    this.flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      va <- a
      vb <- b
    } yield f(va, vb)

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.size)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(a => a)

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(List()))((a, acc) => map2(f(a), acc)(_ :: _))

}