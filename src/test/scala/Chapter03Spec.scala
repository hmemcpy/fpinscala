import ch03.Chapter03
import ch03.Chapter03._
import org.specs2.mutable.Specification

class Chapter03Spec extends Specification {
  val list = List(1, 2, 3)

  "tail" >> {
    tail(list) must_=== List(2, 3)
    tail(Nil) must throwA[NoSuchElementException]
  }

  "setHead" >> {
    setHead(list, 5) must_=== List(5, 2, 3)
    setHead(Nil, 5) must throwA[NoSuchElementException]
  }

  "drop" >> {
    drop(list, 2) must_=== List(3)
  }

  "dropWhile" >> {
    dropWhile(list, (_: Int) < 3) must_=== List(3)
  }

  "init" >> {
    init(list) must_=== List(1, 2)
  }

  "length" >> {
    Chapter03.length(list) must_=== 3
  }

  "foldLeft" >> {
    foldLeft(List("a", "b", "c"), "")((acc, i) => acc + i) must_=== "abc"
  }

  "sum" >> {
    sum(list) must_=== 6
  }

  "product" >> {
    product(List(2, 2, 2)) must_=== 8
  }

  "reverse" >> {
    reverse(list) must_=== List(3, 2, 1)
  }

  "foldRight via foldLeft" >> {
    foldRightViaFoldLeft(List("a", "b", "c"), "")(_ + _) must_=== "abc"
  }

  "foldLeft via foldRight" >> {
    foldLeftViaFoldRight(List("a", "b", "c"), "")(_ + _) must_=== "abc"
  }

  "append" >> {
    append(list, List(6)) must_=== List(1,2,3,6)
  }

  "concat" >> {
    concat(List(List(1), List(2,3))) must_=== list
  }

  "add1" >> {
    add1(list) must_=== List(2,3,4)
  }

  "doubleToString" >> {
    doubleToString(List(1.0, 2.0, 66.1)) must_=== List("1.0", "2.0", "66.1")
  }

  "map" >> {
    Chapter03.map(list)(_.toString) must_=== List("1", "2", "3")
  }

  "filter" >> {
    filter(list)(_ == 2) must_=== List(2)
  }

  "flatMap" >> {
    flatMap(list)(i => List(i,i)) must_=== List(1,1,2,2,3,3)
  }

  "filter via flatMap" >> {
    flatFilter(list)(_ > 1) must_=== List(2,3)
  }

  "add lists" >> {
    addLists(List(1,2,3), List(4,5,6)) must_=== List(5 ,7 ,9)
  }

  "zipWith" >> {
    zipWith(List(1,2,3), List(4,5,6))(_*_) must_=== List(4, 10, 18)
  }

  "hasSub" >> {
    hasSubsequence(List(1,2,3), List(1,2)) must beTrue
    hasSubsequence(List(1,2,3), List(1,2,3)) must beTrue
    hasSubsequence(List(1,2,3), List(2,3)) must beTrue
    hasSubsequence(List(1,2,3), Nil) must beTrue
    hasSubsequence(List(1,2,3), List(3, 1)) must beFalse
  }
}
