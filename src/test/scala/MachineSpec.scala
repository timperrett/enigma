package enigma

import org.scalacheck._
import Prop._

object MachineSpec extends Properties("machine"){
  import Rotors._, Reflectors._

  val enigma = Machine(
    plugboard = Plugboard(Alphabet.shuffled),
    right = I('A'),
    middle = II('A'),
    left = III('A'),
    reflector = Reflectors.B
  )

  property("full scramble") = secure {
    println(">> "+ enigma.use('C'))
    true
  }
}
