import org.specs2.mutable.Specification
import ch04._

//noinspection Specs2Matchers
class Chapter04Spec extends Specification {
  "Option" should {
    import ch04.Option._

    "map" >> {
      Some(1).map(_ + 5) must_=== Some(6)
    }

    "flatMap" >> {
      Some(1).flatMap(i => Some(i + 5)) must_=== Some(6)
    }

    "getOrElse" >> {
      Some(5).getOrElse("test") must_=== 5
      None.getOrElse("test") must_=== "test"
    }

    "orElse" >> {
      Some(1).orElse(Some(2)) must_=== Some(1)
      None.orElse(Some("Hello")) must_=== Some("Hello")
    }

    "filter" >> {
      Some("abcd").filter(_.startsWith("a")) must_=== Some("abcd")
      Some("abcd").filter(_.startsWith("z")) must_=== None
    }

    "map2" >> {
      map2(Some(2), Some("abc"))((i, str) => i + str.length) must_=== Some(5)
    }

    "sequence" >> {
      sequence(List(Some(1))) must_=== Some(List(1))
    }

    "traverse" >> {
      traverse(List("1"))(s => Some(s.toInt)) must_=== Some(List(1))
    }
  }

  "Either" should {
    import ch04.Either._

    "map" >> {
      Right(2).map(_.toString) must_=== Right("2")
      Left(1).map(_.toString) must_=== Left(1)
    }

    "flatMap" >> {
      Right(2).flatMap(i => Right(2 * i)) must_=== Right(4)
      Left("").flatMap(_ => Left("boom")) must_=== Left("")
    }

    "orElse" >> {
      Right(2).orElse(Right(3)) must_=== Right(2)
      Left(1).orElse(Right(3)) must_=== Right(3)
    }

    "map2" >> {
      Right(2).map2(Right(3))(_ + _) must_=== Right(5)
      Right(2).map2(Left(1))(_ + _) must_=== Left(1)
    }

    "traverse" >> {
      traverse(List(2))(i => Right(i.toString)) must_=== Right(List("2"))
      traverse(List(2))(_ => Left(0)) must_=== Left(0)
    }
  }
}
