import ch05.Stream._
import ch05._
import org.specs2.mutable.Specification

//noinspection Specs2Matchers
class Chapter05Spec extends Specification {
  "Stream" should {
    val s = Stream(1, 2, 3, 4, 5)

    "toList empty" >> {
      Stream.empty[Int].toList must_=== List()
    }
    "toList" >> {
      s.toList must_=== List(1, 2, 3, 4, 5)
    }
    "take" >> {
      s.take(2) must_=== Stream(1, 2)
    }
    "drop" >> {
      s.drop(3) must_=== Stream(4, 5)
    }
    "takeWhile" >> {
      s.takeWhile(_ < 4).toList must_=== Stream(1, 2, 3).toList
    }
    "forAll" >> {
      Stream.empty[Int].forAll(_ => true) must beTrue
      Stream("a", "b", "c", "d").forAll(_.length == 1) must beTrue
    }
    "takeWhile via foldRight" >> {
      s.takeWhile(_ < 4).toList must_=== Stream(1, 2, 3).toList
    }
    "headOption via foldRight" >> {
      Stream.empty[Int].headOption_foldRight must beNone
      s.headOption_foldRight must beSome(1)
    }
    "map" >> {
      s.map(_.toString).toList must_=== List("1", "2", "3", "4", "5")
    }
    "filter" >> {
      s.filter(_ % 2 == 0).toList must_=== List(2, 4)
    }
    "append" >> {
      Stream.empty[Int].append(Stream(1, 2)).toList must_=== List(1, 2)
    }
    "flatMap" >> {
      s.flatMap(e => Stream(e.toString)).toList must_=== List("1", "2", "3", "4", "5")
    }
    "constant" >> {
      constant(1).take(5).toList must_=== List(1, 1, 1, 1, 1)
    }
    "from" >> {
      from(5).take(5).toList must_=== List(5, 6, 7, 8, 9)
    }
    "unfold" >> {
      Stream.unfold(0)(n => Some((n, n + 1))).take(5).toList must_=== List(0, 1, 2, 3, 4)
    }
    "fibs via unfold" >> {
      fibs_unfold.take(10).toList must_=== List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    }
    "from via unfold" >> {
      from_unfold(0).take(5).toList must_=== List(0, 1, 2, 3, 4)
    }
    "constant via unfold" >> {
      constant_unfold(1).take(5).toList must_=== List(1, 1, 1, 1, 1)
    }
    "ones via unfold" >> {
      ones_unfold.take(10).toList must_=== List.fill(10)(1)
    }
  }
}
