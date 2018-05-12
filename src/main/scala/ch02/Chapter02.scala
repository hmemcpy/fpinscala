package ch02

import scala.annotation.tailrec

object Chapter02 {

  // 2.1
  // Write a recursive function to get the nth Fibonacci number
  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, previous: Int, current: Int): Int =
      if (n == 0) previous
      else loop(n - 1, current, previous + current)
    loop(n, 0, 1)
  }

  // 2.2
  // Implement `isSorted`, which checks whether an `Array[A]` is sorted
  // according to a given comparison function:
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean =
      if (n + 1 >= as.length) true
      else if (ordered(as(n), as(n + 1))) loop(n + 1)
      else false
    loop(0)
  }

  // 2.3
  // Implement currying
  def curry[A, B, C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  // 2.4
  // Implement uncurry
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  // 2.5
  // Implement function composition
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
