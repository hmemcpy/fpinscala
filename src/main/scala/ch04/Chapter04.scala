package ch04

object Chapter04 {
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

  def variance(xs: Seq[Double]): Option[Double] = ???

}
