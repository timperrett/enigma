package enigma

import org.scalacheck._
import Prop._

object PlugboardSpec extends Properties("plugboard") {
  val p1 = Plugboard(Alphabet.empty)
  val p2 = Plugboard(Alphabet.shuffled)
  val alpha = Gen.choose('A','Z')

  property("identity plugboard") = forAll(alpha){ (c: Char) =>
    p1.transform(c) == c
  }

  property("bijective resolution") = forAll(alpha){ (c: Char) =>
    val a: Char = p2.transform(c)
    val b: Char = p2.transform(a)
    b == c
  }
}
