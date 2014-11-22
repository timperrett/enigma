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
      m <- get[Machine] // use the settings, then modify
      _ <- modify((m: Machine) => update(l)(m))
      _  = println(s"right = ${m.right.offset}, middle = ${m.middle.offset}, left = ${m.left.offset}")
    } yield f(m)(c)
  }

  def forward(r0: Char): State[Machine, Char] =
    for {
      r1 <- step(r0, rightL, _.right.forward)
      _   = println(s"r1 = $r1")

      r2 <- step(r1, middleL, _.middle.forward)
      _   = println(s"r2 = $r2")

      r3 <- step(r2, leftL, _.left.forward)
      _   = println(s"r3 = $r3")
    } yield r3

  def reverse(r0: Char): State[Machine, Char] =
    for {
      r1 <- step(r0, leftL, _.left.forward)
      _   = println(s"r1 = $r1")

      r2 <- step(r1, middleL, _.middle.forward)
      _   = println(s"r2 = $r2")

      r3 <- step(r2, rightL, _.right.reverse)
      _   = println(s"r3 = $r3")
    } yield r3

  def use(c: Char): State[Machine, Char] =
    for {
      m <- get[Machine]
      b <- state(m.plugboard.transform(c))
      c <- forward(c)
      d <- state(m.reflector.transform(c))
      e <- reverse(d)
      f <- state(m.plugboard.transform(c))
    } yield f
}

/////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////// EXAMPLE //////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////

// object main extends App {
//   
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