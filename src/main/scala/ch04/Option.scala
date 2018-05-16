package ch04

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case Some(v) => Some(f(v))
      case _ => None
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case Some(v) => v
      case _ => default
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(v => if (f(v)) Some(v) else None)
}
final case class Some[A](value: A) extends Option[A]
final case object None extends Option[Nothing]

object Option {
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b.map(bb => f(aa, bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))
}
