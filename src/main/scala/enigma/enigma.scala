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

// given a 26 char string, turn that into a resolvable table lookup
trait WiringTable {
  // stupid validation, but good enough.
  assert(table.length == 26)

  lazy val mapping: Map[Char,Char] =
    table.zipWithIndex.map { case (c,i) => Alphabet.ordered(i) -> c }.toMap

  def table: String

  def transform(c: Char): Char =
    mapping(c) // throws an exception if invalid character; mappings should be total

}

// primary difference here is that `Reflector` has no notch, and is always configured
// with symetric wiring tables. i.e. A -> Z, Z -> A. Reflectors also do not have
// configurable rings; they were fixed and came in preset variants: A, B and C
// Known as Umkehrwalze in german.
case class Reflector(table: String) extends WiringTable

case class Rotor(
  table: String,
  offset: Char, // the posistion the alphabet ring is currently rotated too
  ring: Char, // Ringstellung: posistion of the wiring relative to the offset
  notch: Char // sometimes called ground setting
) extends WiringTable {

  // Where rotor I in the A-position normally encodes an A into an E,
  // with a ring setting offset B it will be encoded into K
  override def transform(c: Char): Char =
    mapping(offset)

  def hasReachedNotch: Boolean =
    notch == Alphabet.nextLetter(offset)
}

import scalaz.syntax.state._
import scalaz.State, State._

object Rotor {

  // "install" the rotor with the given settings; these change based
  // on the setup the sender and reciever are using to encipher messages
  // def setup: State[Rotor, Rotor] =
  //   for {
  //     s <- init
  //     x <- modify((z: Rotor) => z)
  //     r <- get
  //   } yield r

  // this simulates the rotor actually moving along its states by a
  // single letter in the alphabet.
  // def step(r: Rotor): State[Rotor, Rotor] =
  //   for {
  //     _ <- modify((z: Rotor) => r.copy(ring = Alphabet.nextLetter(z.ring)))
  //     n <- get
  //   } yield n
}

case class Machine(
  plugboard: Plugboard,
  // static: Rotor, // not sure if this is needed?
  right: Rotor,
  middle: Rotor,
  left: Rotor,
  reflector: Reflector
)

object Machine {
  import monocle.{Lenser,Lenses,Lens}
  import monocle.syntax._

  type RotorLens = Lens[Machine, Machine, Rotor, Rotor]

  val lenserM = Lenser[Machine]
  val rightL = lenserM(_.right)
  val middleL = lenserM(_.middle)
  val leftL = lenserM(_.left)

  val lenserR = Lenser[Rotor]
  val rotorL: Lens[Rotor,Rotor,Char,Char] = lenserR(_.offset)

  def step(c: Char, l: RotorLens, f: Machine => Char => Char): State[Machine, Char] = {
    def update(rl: RotorLens)(m: Machine): Machine =
      m |-> rl |-> rotorL modify(Alphabet.nextLetter)

    for {
      m <- get[Machine] // use the settings, then modify
      _ <- modify((m: Machine) => update(l)(m))
      _  = println(s"right = ${m.right.offset}, middle = ${m.middle.offset}, left = ${m.left.offset}")
    } yield f(m)(c)
  }

  def scramble(r0: Char): State[Machine, Char] =
    for {
      r1 <- step(r0, rightL, _.right.transform)
      _   = println(s"r1 = $r1")

      r2 <- step(r1, middleL, _.middle.transform)
      _   = println(s"r2 = $r2")

      r3 <- step(r2, leftL, _.left.transform)
      _   = println(s"r3 = $r3")
    } yield r3

  def foooo(c: Char): State[Machine, Char] =
    for {
      o1 <- scramble(c)
    } yield o1

  // def use(c: Char): Machine => Char = m =>
  //   m.plugboard.transform(c) // |>


}

/////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////// EXAMPLE //////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////

// Actual configurations used by the Nazi's both before and during the war
// from the Enigma I & M3 army/navy machines:
// http://en.wikipedia.org/wiki/Enigma_rotor_details#Rotor_wiring_tables
object Rotors {
  def I(p: Char) = Rotor(
    table  = "EKMFLGDQVZNTOWYHXUSPAIBRCJ",
    notch  = 'Q',
    offset = p
  )
  def II(p: Char) = Rotor(
    table  = "AJDKSIRUXBLHWTMCQGZNPYFVOE",
    notch  = 'E',
    offset = p
  )
  def III(p: Char) = Rotor(
    table  = "BDFHJLCPRTXVZNYEIWGAKMUSQO",
    notch  = 'V',
    offset = p
  )
  def IV(p: Char) = Rotor(
    table  = "ESOVPZJAYQUIRHXLNFTGKDCMWB",
    notch  = 'J',
    offset = p
  )
  def V(p: Char) = Rotor(
    table  = "VZBRGITYUPSDNHLXAWMJQOFECK",
    notch  = 'Z',
    offset = p
  )
}

object Reflectors {
  val A = Reflector("EJMZALYXVBWFCRQUONTSPIKHGD")
  val B = Reflector("YRUHQSLDPXNGOKMIEBFZCWVJAT")
  val C = Reflector("FVPJIAOYEDRZXWGCTKUQSBNMHL")
}

object main extends App {
  import Rotors._

  val p = Plugboard(Alphabet.shuffled)

  val m = Machine(
    plugboard = p,
    right = I('A'),
    middle = II('A'),
    left = III('A'),
    reflector = Reflectors.B
  )

  // to make this work, gotta adjust the implementation of Rotor
  // such that it actually makes use of the state of its ring when
  // computing the next character
  val exe: State[Machine,String] = for {
    _ <- init
    a <- Machine.foooo('A')
    b <- Machine.foooo('A')
  } yield s"a = $a, b = $b"

  exe.eval(m)
}