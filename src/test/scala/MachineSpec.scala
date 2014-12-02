package enigma

import org.scalacheck._
import Prop._

object MachineSpec extends Properties("machine"){
  import Rotors._, Reflectors._

  val e1 = Machine(
    plugboard = Plugboard(Alphabet.ordered),
    left = I('A'),
    middle = II('A'),
    right = III('A'),
    reflector = Reflectors.B
  )

  property("full scramble") = secure {
    // println(">> "+ Machine.forward('K').gogogo)

    // println("{{{ " + Machine.rotorL.asGetter)

    println {
      e1.use('A')
    }

    true
  }
}
