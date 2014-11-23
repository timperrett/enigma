package enigma

import scalaz.syntax.state._
import scalaz.State, State._

case class Machine(
  plugboard: Plugboard,
  right: Rotor,
  middle: Rotor,
  left: Rotor,
  reflector: Reflector
){
  // syntax for state monad application
  def use(c: Char): Char =
    Machine.use(c).eval(this)
}

object Machine {
  import monocle.{Lenser,Lenses,Lens}
  import monocle.syntax._

  type RotorLens = Lens[Machine, Machine, Rotor, Rotor]

  val lenserM = Lenser[Machine]
  val rightL = lenserM(_.right)
  val middleL = lenserM(_.middle)
  val leftL = lenserM(_.left)

  val lenserR = Lenser[Rotor]
  val rotorL: Lens[Rotor,Rotor,Char,Char] = lenserR(_.posistion)

  private def step(c: Char, l: RotorLens, f: Machine => Char => Char): State[Machine, Char] = {
    def update(rl: RotorLens)(m: Machine): Machine =
      m |-> rl |-> rotorL modify(Alphabet.nextLetter)

    for {
      _ <- get[Machine] // use the settings, then modify
      _ <- modify((m: Machine) => update(l)(m))
      m <- get[Machine]
      _  = println(s"right = ${m.right.offset}, middle = ${m.middle.offset}, left = ${m.left.offset}")
    } yield f(m)(c)
  }

  private[enigma] def forward(r0: Char): State[Machine, Char] =
    for {
      r1 <- step(r0, rightL, _.right.forward)
      _   = println(s"f1 = $r1")

      r2 <- step(r1, middleL, _.middle.forward)
      _   = println(s"f2 = $r2")

      r3 <- step(r2, leftL, _.left.forward)
      _   = println(s"f3 = $r3")
    } yield r3

  private[enigma] def reverse(r0: Char): State[Machine, Char] =
    for {
      r1 <- step(r0, leftL, _.left.forward)
      _   = println(s"r1 = $r1")

      r2 <- step(r1, middleL, _.middle.forward)
      _   = println(s"r2 = $r2")

      r3 <- step(r2, rightL, _.right.reverse)
      _   = println(s"r3 = $r3")
    } yield r3

  def scramble(c0: Char): State[Machine,Char] =
    for {
      a <- forward(c0)
      _  = println("1 ::: " + a)

      m <- get[Machine]
      b <- state(m.reflector.transform(a))
      _  = println("2 ::: " + b)

      c <- reverse(b)
      _  = println("3 ::: " + c)
    } yield c

  def use(c0: Char): State[Machine, Char] =
    for {
      m <- get[Machine]
      a <- state(m.plugboard.transform(c0))
      b <- scramble(a)
      c <- state(m.plugboard.transform(b))
    } yield c
}
