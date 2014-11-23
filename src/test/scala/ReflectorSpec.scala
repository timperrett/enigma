package enigma

import org.scalacheck._
import Prop._

object ReflectorSpec extends Properties("reflector") {
  import Reflectors._

  // check going back and forth is legit
  property("reflectors") = secure {
    B.transform('A') == 'Y' &&
    B.transform('Y') == 'A'
  }

}