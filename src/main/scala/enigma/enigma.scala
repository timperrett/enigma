package enigma

// import scalaz._, Scalaz._
import scalaz.syntax.state._
import scalaz.State, State._

// some convenience as we are primarily working with letters A through Z.
object Alphabet {
  val ordered: Seq[Char] = ('A' to 'Z').toList

  // compute length of the alphabet so that the code reads nicer and
  // we're not doing .length all over the place. For syntax only.
  val length: Int = ordered.length

  // produce a randomly shuffled sequence of the alphabet.
  def shuffled: Seq[Char] =
    util.Random.shuffle(ordered).toList

  def empty: Seq[Char] = Seq.empty

  // imperitive, but it works. probally a better way.
  def nextLetter(after: Char): Char = {
    val i = ordered.indexOf(after)
    if(i == (length-1) || i == -1) ordered.head
    else ordered.apply(i+1)
  }
}

/**
 * Here the signal is connected to the 'T' input on the plugboard. Some of
 * the letters on the plugboard will be wired up to other letters (the plugs),
 * causing the signal to be diverted. If the 'T' input is not plugged to another
 * letter then our signal will pass straight to the 'T output. In our case, though
 * the 'T' is plugged to the 'K', so the signal is diverted to a new path, the
 * letter is now 'K'.
 */
case class Plugboard(shuffled: Seq[Char]){
  // take the shuffled input, split it in half, interleave the halves and then
  // make sure that A -> B and B -> A such that the operations on the plugboard
  // are deterministic back and forth.
  private val mapping: Map[Char,Char] =
    shuffled.splitAt(Alphabet.length / 2
      ).zipped.flatMap((a,b) => Seq(a -> b, b -> a)).toMap

  def transform(c: Char): Char =
    mapping.get(c).getOrElse(c)
}

// can generalise this as it seems to functionally do the same as `Plugboard`?
// thinking this is primialry seperate in the machine itself because of wiring
// and EE constraints of the real world?
case class Reflector(mapping: Map[Char,Char]){
  def transform(c: Char): Char =
    mapping.get(c).getOrElse(c)
}

case class Rotor(
  outer: Contacts,
  inner: Contacts,
  notch: Char
){
  def transform(c: Char): Char =
    outer.andThen(inner).apply(c)
}

object Rotor {

  // "install" the rotor with the given settings; these change based
  // on the setup the sender and reciever are using to encipher messages
  def starting(at: Char, r: Rotor): State[Posistion, Rotor] =
    for {
      s <- init
      _ <- modify((c: Posistion) => at)
    } yield r

  // this simulates the rotor actually moving along its states by a
  // single letter in the alphabet.
  def step: State[Posistion, Rotor] => State[Posistion, Rotor] = s =>
    for {
      r <- s
      _ <- modify((c: Posistion) => Alphabet.nextLetter(c))
    } yield r
}

case class Machine(
  plugboard: Plugboard,
  static: Rotor,
  right: Rotor,
  middle: Rotor,
  left: Rotor,
  reflector: Reflector
){

}

object Machine {

  // def setup(m: Machine): State[Machine, Machine] =
  //   for {
  //     s <- init

  //     // _ <- modify((m: Machine) => )
  //   }


  def use(c: Char): Machine => Char = m =>
    m.plugboard.transform(c) // |>


}

/////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////// EXAMPLE //////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////

// object Rotors {
//   val I = Rotor(
//     outer = identity,
//     inner = identity,
//     notch = 'O'
//   )
// }

object main extends App {
  // import Rotors._

  val p = Plugboard(Alphabet.shuffled)

  // def powerup: State[Machine, Char] =
  //   for {
  //     a <- init
  //     _ <- modify((s: Machine) => s)
  //     o <- get
  //   } yield

  // val r = I :: I :: I :: Nil

  println(s">>>>> $p")

}