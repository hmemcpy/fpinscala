import ch02.Chapter02._
import org.specs2.mutable.Specification

class Chapter02Spec extends Specification {
  "fib" >> {
    0 to 10 map fib must_=== Vector(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
  }

  "isSorted" >> {
    isSorted(Array(1, 2), (x: Int, y: Int) => x < y) must beTrue
    isSorted(Array(1, 1, 2), (x: Int, y: Int) => x <= y) must beTrue
    isSorted(Array(1, 1, 2), (x: Int, y: Int) => x > y) must beFalse
    isSorted(Array('a', 'b', 'c'), (x: Char, y: Char) => x < y) must beTrue
    isSorted(Array('a', 'b', 'c'), (x: Char, y: Char) => x == y) must beFalse
  }

  def add(x: Int, y: Int) = x + y

  "curry" >> {
    curry(add)(2)(3) must_=== add(2, 3)
  }

  "uncurry" >> {
    uncurry(curry(add))(2, 3) must_=== add(2, 3)
  }

  def isEven(i: Int) = i % 2 == 0
  def length(s: String) = s.length

  "compose" >> {
    compose(isEven, length)("abc") must_=== isEven(length("abc"))
  }
}
