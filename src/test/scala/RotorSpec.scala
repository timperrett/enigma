package enigma

import org.scalacheck._
import Prop._

object RotorSpec extends Properties("rotor") {
  import Rotors._

  val p = Plugboard(Alphabet.shuffled)

  val m1 = Machine(
    plugboard = p,
    right = I('A'),
    middle = II('A'),
    left = III('A'),
    reflector = Reflectors.B
  )

  /**
   * When a rotor has stepped, you must take into account the offset to know what
   * the output is, and where it enters the next rotor. If for example rotor I is
   * in the B-position, an A enters at the letter B which is wired to the K.
   * Because of the offset this K enters the next rotor in the J position.
   */
  property("manual: rotor & reflector transform") = secure {
    // start = A, offset = A, ring = A
    val r1 = I('A')
    // start = A, offset = B, ring = A
    val r2 = I('A').copy(offset = 'B')
    // start = A, offset = A, ring = B
    val r3 = I('A').copy(ring = 'B')

    val l = Reflectors.B

    r1.transform('A') == 'E' &&
    r1.transform('B') == 'K' &&
    r1.transform('K') == 'N' &&
    l.transform('A') == 'Y' &&
    l.transform('Y') == 'A' &&
    r2.transform('A') == 'K'
    r3.transform('A') == 'K'
  }


}

