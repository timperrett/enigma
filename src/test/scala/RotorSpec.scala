package enigma

import org.scalacheck._
import Prop._

object RotorSpec extends Properties("rotor") {
  import Rotors._

  //////////////////////// SINGLE ROTOR ///////////////////////////

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
    val r2 = I('A').copy(position = 'B')
    // start = A, offset = A, ring = B
    val r3 = I('A').copy(ring = 'B')

    val a1 = III('A')
    val a2 = II('A')
    val a3 = I('A')

    a1.forward('A') == 'B' &&
    a2.forward('B') == 'J' &&
    a3.forward('J') == 'Z' &&
    // reflector B changes Z -> T
    a3.reverse('T') == 'L' &&
    a2.reverse('L') == 'K' &&
    a1.reverse('K') == 'U'

    r1.forward('A') == 'E' &&
    r1.forward('B') == 'K' &&
    r1.forward('K') == 'N' &&
    r2.forward('A') == 'J' &&
    r3.forward('A') == 'K'

  }

}

