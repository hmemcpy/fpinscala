package ch05

import scala.annotation.tailrec

sealed trait Stream[+A] {
  import Stream._

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  // 5.1
  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

  // 5.2
  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case _ => empty
    }

  @tailrec
  final def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
      case _ => empty
    }

  // 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, t) => p(h) && t)

  // 5.5
  def takeWhile_foldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t takeWhile p) else empty)

  // 5.6
  def headOption_foldRight: Option[A] =
    foldRight[Option[A]](None)((h, _) => Some(h))

  // 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](as: => Stream[B]): Stream[B] =
    foldRight(as)((a, b) => cons(a, b))

  def concat[A](a: Stream[Stream[A]]): Stream[A] =
    a.foldRight(empty[A])((h, t) => h append t)

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    concat(map(f))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  // 5.8
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  // 5.9
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _ => empty
    }

  // 5.12
  val fibs_unfold: Stream[Int] =
    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  def from_unfold(n: Int): Stream[Int] =
    unfold(0)(n => Some(n, n + 1))

  def constant_unfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  val ones_unfold: Stream[Int] =
    constant_unfold(1)

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
