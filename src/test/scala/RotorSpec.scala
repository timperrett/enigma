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

  property("manual: rotor & reflector transform") = secure {
    val r = I('A')
    val l = Reflectors.B
    r.transform('A') == 'E' &&
    r.transform('B') == 'K' &&
    r.transform('K') == 'N' &&
    l.transform('A') == 'Y' &&
    l.transform('Y') == 'A'
  }

  /**
   * When a rotor has stepped, you must take into account the offset to know what
   * the output is, and where it enters the next rotor. If for example rotor I is
   * in the B-position, an A enters at the letter B which is wired to the K.
   * Because of the offset this K enters the next rotor in the J position.
   */
  property("foo") = secure {



    val r = I('A').copy(offset = 'B')
    println(">>>>>>> " + r.transform('A'))
    true
  }


  // Machine.step('A', Machine.rightL, _.right.transform).eval(m1) == 'E' &&
  // Machine.step('B', Machine.rightL, _.right.transform).eval(m1) == 'K' //&&
  // Machine.step('K', Machine.rightL, _.right.transform).eval(m1) == 'N'
}

