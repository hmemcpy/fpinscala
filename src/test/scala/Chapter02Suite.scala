import Chapter02._
import minitest._

object Chapter02Suite extends SimpleTestSuite {
  test("fib") {
    assertEquals(0 to 10 map fib, Vector(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55))
  }

  test("isSorted") {
    assert(isSorted(Array(1, 2), (x: Int, y: Int) => x < y))
    assert(isSorted(Array(1, 1, 2), (x: Int, y: Int) => x <= y))
    assert(!isSorted(Array(1, 1, 2), (x: Int, y: Int) => x > y))
    assert(isSorted(Array('a', 'b', 'c'), (x: Char, y: Char) => x < y))
    assert(!isSorted(Array('a', 'b', 'c'), (x: Char, y: Char) => x == y))
  }

  def add(x: Int, y: Int) = x + y

  test("curry") {
    assert(curry(add)(2)(3) == add(2, 3))
  }

  test("uncurry") {
    assert(uncurry(curry(add))(2, 3) == add(2, 3))
  }

  def isEven(i: Int) = i % 2 == 0
  def length(s: String) = s.length

  test("compose") {
    assert(compose(isEven, length)("abc") == isEven(length("abc")))
  }
}
