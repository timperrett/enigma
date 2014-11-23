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

  import scalaz.syntax.state._
  import scalaz.State, State._

  implicit class syntax(s: State[Machine,Char]){
    def gogogo: Char =
      s.eval(e1)
  }

  property("full scramble") = secure {
    println(">> "+ Machine.forward('K').gogogo)
    true
  }
}
