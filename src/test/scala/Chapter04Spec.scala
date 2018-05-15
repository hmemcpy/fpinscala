import org.specs2.mutable.Specification
import ch04.Chapter04._

class Chapter04Spec extends Specification {
  "Option.Some" should {
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

}
