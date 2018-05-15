import org.specs2.mutable.Specification
import ch04.Chapter04._

import scala.util.Try

class Chapter04Spec extends Specification {
  "Option" should {
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
