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
  ring: Char, // the starting posistion the ring was set too
  notch: Char // sometimes called ground setting
){
  def transform(c: Char): Char =
    outer.andThen(inner).apply(c)

  def hasReachedNotch: Boolean =
    notch == Alphabet.nextLetter(ring)
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
  val rotorL: Lens[Rotor,Rotor,Char,Char] = lenserR(_.ring)

  def step(c: Char, l: RotorLens): State[Machine, Char] = {
    def update(rl: RotorLens)(m: Machine): Machine =
      m |-> rl |-> rotorL modify(Alphabet.nextLetter)

    for {
      _ <- modify((m: Machine) => update(l)(m))
      m <- get[Machine]
    } yield m.right.transform(c)
  }

  def scramble(r0: Char): State[Machine, Char] =
    for {
      r1 <- step(r0, rightL)
      r2 <- step(r1, middleL)
      r3 <- step(r2, leftL)
    } yield r3

  //   // rotorL.modify(c => Alphabet.nextLetter(c))
  // }



  // def scramble(r1: Rotor)  = {
  //   // if(r1.hasReachedNotch)
  //   //   ...
  //   // else
  //     r1.transform()
  // }

  // def setup(m: Machine): State[Machine, Machine] =
  //   for {
  //     s <- init
  //     // r1 <- Rotor.setup(m.right)
  //     // r2 <- Rotor.setup(m.middle)
  //     // r3 <- Rotor.setup(m.left)
  //     // _ <- modify((m: Machine) => )
  //   } yield m


  // def use(c: Char): Machine => Char = m =>
  //   m.plugboard.transform(c) // |>


}

/////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////// EXAMPLE //////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////

object Rotors {
  val I = Rotor(
    outer = identity,
    inner = identity,
    ring  = 'A',
    notch = 'O'
  )
}

object main extends App {
  import Rotors._

  val p = Plugboard(Alphabet.shuffled)

  // val program = for {
  //   r  <- Rotor.setup
  //   d1 <- get
  //   _  <- Rotor.step(r)
  //   d2 <- get
  //   // _  = println(d2)
  //   _  <- Rotor.step(r)
  //   d3 <- get
  //   // _  = println(d3)
  // } yield d1 :: d2 :: d3 :: Nil

  // program.eval(I).zipWithIndex.foreach { case (v,i) =>
  //   println(s"r$i - $v")
  // }

  // def powerup: State[Machine, Char] =
  //   for {
  //     a <- init
  //     _ <- modify((s: Machine) => s)
  //     o <- get
  //   } yield

  // val r = I :: I :: I :: Nil

  // println(s">>>>> $p")

}