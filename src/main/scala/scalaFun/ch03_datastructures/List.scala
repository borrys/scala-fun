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
    def dropLast(xs: List[A], acc:List[A] =List()): List[A] = {
      xs match {
        case Nil => sys.error("init on empty list")
        case Cons(_, Nil) => acc
        case Cons(a, t) => dropLast(t, Cons(a, acc))
      }
    }

    @tailrec
    def reverse(xs: List[A], acc:List[A]):List[A] = {
      xs match {
        case Nil => acc
        case Cons(a, t) => reverse(t, Cons(a, acc))
      }
    }

    val allButLast = dropLast(l)
    reverse(allButLast, List())
  }
}