package ch03

import scala.annotation.tailrec

object Chapter03 {
  sealed trait List[+A]
  final case object Nil extends List[Nothing]
  final case class Cons[A](x: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      case Nil => z
    }

  // 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, tail) => tail
    case Nil => throw new NoSuchElementException
  }

  // 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, tail) => Cons(h, tail)
    case Nil => throw new NoSuchElementException
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else
      l match {
        case Cons(_, tail) => drop(tail, n - 1)
        case Nil => Nil
      }
  }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, tail) if f(h) => dropWhile(tail, f)
    case _ => l
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(h, tail) => Cons(h, init(tail))
    case Nil => throw new NoSuchElementException
  }

  // 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)

  // 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      case Nil => z
    }

  // 3.11
  def sum(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
  def product(as: List[Int]): Int = foldLeft(as, 1)(_ * _)

  // 3.12
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((acc, i) => Cons(i, acc))

  // 3.13*
  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b: B, a: A) => f(a, b))

  // 3.14
  def append[A, B](as: List[A], as2: List[A]): List[A] =
    foldRight(as, as2)((a, b) => Cons(a, b))

  // 3.15*
  def concat[A](lss: List[List[A]]): List[A] =
    foldRight(lss, Nil: List[A])(append)

  // 3.16
  def add1(as: List[Int]): List[Int] =
    foldRight(as, Nil: List[Int])((a, b) => Cons(a + 1, b))

  // 3.17
  def doubleToString(as: List[Double]): List[String] =
    foldRight(as, Nil: List[String])((a, b) => Cons(a.toString, b))

  // 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  // 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  // 3.21
  def flatFilter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  // 3.22
  def addLists(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(ha, ta), Cons(hb, tb)) => Cons(ha + hb, addLists(ta, tb))
  }

  // 3.23
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
  }

  // 3.24*
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def startsWith[A](l: List[A], p: List[A]): Boolean = (l, p) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
      case _ => false
    }
    sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_, t) => hasSubsequence(t, sub)
    }
  }
}
