package scalaFun.ch03_datastructures

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("setHead on empty list")
      case Cons(_, t) => Cons(h, t)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => sys.error("not enough elements in list")
      case Cons(_, t) => drop(t, n - 1)
    }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(a, t) if f(a) => dropWhile(t)(f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] = {
    @tailrec
    def dropLast(xs: List[A], acc: List[A] = List()): List[A] = {
      xs match {
        case Nil => sys.error("init on empty list")
        case Cons(_, Nil) => acc
        case Cons(a, t) => dropLast(t, Cons(a, acc))
      }
    }

    @tailrec
    def reverse(xs: List[A], acc: List[A]): List[A] = {
      xs match {
        case Nil => acc
        case Cons(a, t) => reverse(t, Cons(a, acc))
      }
    }

    val allButLast = dropLast(l)
    reverse(allButLast, List())
  }

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sumLeft(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def productLeft(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]())((acc, a) => Cons(a, acc))

  def append[A](as: List[A], x: A): List[A] =
    foldRight(as, List(x))(Cons(_, _))

  def concatAll[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])(append)

  def increment(as: List[Int]): List[Int] =
    foldRight(as, Nil: List[Int])((a, acc) => Cons(a + 1, acc))

  def asStrings[A](as: List[A]): List[String] =
    foldRight(as, Nil: List[String])((a, acc) => Cons(a.toString, acc))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A])(p: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, acc) => if (p(a)) Cons(a, acc) else acc)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concatAll(map(as)(f))

  def filterViaFlatMap[A](as: List[A])(p: A => Boolean): List[A] =
    flatMap(as)(a => if (p(a)) List(a) else Nil)

  def sumWith(xs: List[Int], ys: List[Int]): List[Int] =
    (xs, ys) match {
      case (Cons(x, tx), Cons(y, ty)) => Cons(x + y, sumWith(tx, ty))
      case _ => Nil
    }

  def zipWith[A, B, C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] =
    (xs, ys) match {
      case (Cons(x, tx), Cons(y, ty)) => Cons(f(x, y), zipWith(tx, ty)(f))
      case _ => Nil
    }

  @tailrec
  def startsWith[A](xs: List[A], prefix: List[A]): Boolean = {
    (xs, prefix) match {
      case (_, Nil) => true
      case (Cons(x, tx), Cons(p, tp)) if x == p => startsWith(tx, tp)
      case _ => false
    }
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup match {
      case Nil => sub == Nil
      case Cons(_, t) => if (startsWith(sup, sub)) true else hasSubsequence(t, sub)
    }
  }
}