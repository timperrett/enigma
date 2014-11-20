package enigma

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

// primary difference here is that `Reflector` has no notch, and is always configured
// with symetric wiring tables. i.e. A -> Z, Z -> A. Reflectors also do not have
// configurable rings; they were fixed and came in preset variants: A, B and C
// Known as Umkehrwalze in german.
case class Reflector(wiring: String){
  val mapping: Map[Char,Char] =
    wiring.zipWithIndex.map { case (c,i) => Alphabet.ordered(i) -> c }.toMap

  def transform(c: Char): Char =
    mapping(c) // throws an exception if invalid character.
}

case class Rotor(
  wiring: String,  // the mapping of normal A->Z letters to its scrambled form
  ring: Char = 'A', // Ringstellung: posistion of the wiring relative to the offset
  notch: Char,
  posistion: Char // Grundstellung the posistion the alphabet ring is currently rotated too
){
  val forwardMapping: Seq[Char] = wiring.toSeq
  val reverseMapping: Seq[Char] = wiring.reverse.toSeq
  val ringAsInt: Int = ring + 'A'
  val posistionAsInt: Int = 'A' + posistion
  val offset: Int = posistionAsInt - ringAsInt
  val size = wiring.length

  private def adjust(c: Char): Int =
    (size + (c - 'A') + offset) % size

  private def encode(c: Char, w: Seq[Char]): Char = {
    val adjustment = adjust(c)
    val resultOffset = (size + forwardMapping(adjustment) - 'A' - offset) % size
    ('A' + resultOffset).toChar
  }

  def forward(c: Char): Char =
    encode(c, forwardMapping)

  def reverse(c: Char): Char =
    encode(c, reverseMapping)

  def hasReachedNotch: Boolean =
    notch == posistion
}

import scalaz.syntax.state._
import scalaz.State, State._

case class Machine(
  plugboard: Plugboard,
  right: Rotor,
  middle: Rotor,
  left: Rotor,
  reflector: Reflector
)

// object Machine {
//   import monocle.{Lenser,Lenses,Lens}
//   import monocle.syntax._

//   type RotorLens = Lens[Machine, Machine, Rotor, Rotor]

//   val lenserM = Lenser[Machine]
//   val rightL = lenserM(_.right)
//   val middleL = lenserM(_.middle)
//   val leftL = lenserM(_.left)

//   val lenserR = Lenser[Rotor]
//   val rotorL: Lens[Rotor,Rotor,Char,Char] = lenserR(_.offset)

//   def step(c: Char, l: RotorLens, f: Machine => Char => Char): State[Machine, Char] = {
//     def update(rl: RotorLens)(m: Machine): Machine =
//       m |-> rl |-> rotorL modify(Alphabet.nextLetter)

//     for {
//       m <- get[Machine] // use the settings, then modify
//       _ <- modify((m: Machine) => update(l)(m))
//       _  = println(s"right = ${m.right.offset}, middle = ${m.middle.offset}, left = ${m.left.offset}")
//     } yield f(m)(c)
//   }

//   def scramble(r0: Char): State[Machine, Char] =
//     for {
//       r1 <- step(r0, rightL, _.right.transform)
//       _   = println(s"r1 = $r1")

//       r2 <- step(r1, middleL, _.middle.transform)
//       _   = println(s"r2 = $r2")

//       r3 <- step(r2, leftL, _.left.transform)
//       _   = println(s"r3 = $r3")
//     } yield r3

//   def foooo(c: Char): State[Machine, Char] =
//     for {
//       o1 <- scramble(c)
//     } yield o1

//   // def use(c: Char): Machine => Char = m =>
//   //   m.plugboard.transform(c) // |>


// }

// Actual configurations used by the Nazi's both before and during the war
// from the Enigma I & M3 army/navy machines:
// http://en.wikipedia.org/wiki/Enigma_rotor_details#Rotor_wiring_tables
object Rotors {
  def I(p: Char) = Rotor(
    wiring  = "EKMFLGDQVZNTOWYHXUSPAIBRCJ",
    notch  = 'Q',
    ring   = 'A',
    posistion = p
  )
  def II(p: Char) = Rotor(
    wiring  = "AJDKSIRUXBLHWTMCQGZNPYFVOE",
    notch  = 'E',
    ring   = 'A',
    posistion = p
  )
  def III(p: Char) = Rotor(
    wiring  = "BDFHJLCPRTXVZNYEIWGAKMUSQO",
    notch  = 'V',
    ring   = 'A',
    posistion = p
  )
  def IV(p: Char) = Rotor(
    wiring  = "ESOVPZJAYQUIRHXLNFTGKDCMWB",
    notch  = 'J',
    ring   = 'A',
    posistion = p
  )
  def V(p: Char) = Rotor(
    wiring  = "VZBRGITYUPSDNHLXAWMJQOFECK",
    notch  = 'Z',
    ring   = 'A',
    posistion = p
  )
}

object Reflectors {
  val A = Reflector("EJMZALYXVBWFCRQUONTSPIKHGD")
  val B = Reflector("YRUHQSLDPXNGOKMIEBFZCWVJAT")
  val C = Reflector("FVPJIAOYEDRZXWGCTKUQSBNMHL")
}

/////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////// EXAMPLE //////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////

// object main extends App {
//   import Rotors._

//   val p = Plugboard(Alphabet.shuffled)

//   val m = Machine(
//     plugboard = p,
//     right = I('A'),
//     middle = II('A'),
//     left = III('A'),
//     reflector = Reflectors.B
//   )

//   // to make this work, gotta adjust the implementation of Rotor
//   // such that it actually makes use of the state of its ring when
//   // computing the next character
//   val exe: State[Machine,String] = for {
//     _ <- init
//     a <- Machine.foooo('A')
//     b <- Machine.foooo('A')
//   } yield s"a = $a, b = $b"

//   exe.eval(m)
// }