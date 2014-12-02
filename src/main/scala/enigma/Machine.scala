package enigma

import scalaz.syntax.state._
import scalaz.State, State._

case class Machine(
  plugboard: Plugboard,
  right: Rotor,
  middle: Rotor,
  left: Rotor,
  reflector: Reflector
)

object Machine {
  import monocle.{Lenser,Lenses,Lens}
  import monocle.syntax._

  type MachineLens = Lens[Machine, Machine, Rotor, Rotor]

  val lenserM = Lenser[Machine]
  val rightL = lenserM(_.right)
  val middleL = lenserM(_.middle)
  val leftL = lenserM(_.left)

  val lenserR = Lenser[Rotor]
  val rotorL: Lens[Rotor,Rotor,Char,Char] = lenserR(_.posistion)

  def rtl(m: Machine)(c: Char): Char =
    m.right.forward _ andThen m.middle.forward andThen m.left.forward apply(c)

  def ltr(m: Machine)(c: Char): Char =
    m.left.reverse _ andThen m.middle.reverse andThen m.right.reverse apply(c)

  def use(c: Char): State[Machine, Char] = {
    def stepRotor(r: Rotor): Rotor =
      rotorL.modify(r, Alphabet.nextLetter)

    def right(m: Machine): Machine =
      m |-> rightL modify(stepRotor)

    def middle(m: Machine): Machine =
      m |-> middleL modify(r =>
        if(m.right.notch == r.posistion || m.left.notch == r.posistion) stepRotor(r)
        else r)

    def left(m: Machine): Machine =
      m |-> leftL modify(r =>
        if(r.posistion == m.middle.notch) stepRotor(r)
        else r)

    for {
      _ <- get[Machine]
      _ <- modify((m: Machine) => right(m))
      _ <- modify((m: Machine) => middle(m))
      _ <- modify((m: Machine) => left(m))
      o <- get[Machine]
    } yield
      o.plugboard.transform _ andThen
      rtl(o) andThen
      o.reflector.transform andThen
      ltr(o) andThen o.plugboard.transform apply(c)
  }



  // private[enigma] def step(c: Char, l: MachineLens, f: Machine => Char => Char): State[Machine, Char] = {
  //   def update(ml: MachineLens)(m: Machine): Machine =
  //     m |-> ml |-> rotorL modify(Alphabet.nextLetter)

  //   for {
  //     _ <- get[Machine] // use the settings, then modify
  //     _ <- modify((m: Machine) => update(l)(m))
  //     m <- get[Machine]
  //     _  = println(s"right = ${m.right.offset}, middle = ${m.middle.offset}, left = ${m.left.offset}")
  //   } yield f(m)(c)
  // }

  // private def check(ml: MachineLens)(m: Machine): Boolean =
  //   ml.asGetter.get(m).hasReachedNotch

  // private[enigma] def forward(r0: Char): State[Machine, Char] =
  //   for {
  //     f1 <- step(r0, rightL, _.right.forward)
  //     _   = println(s"f1 = $f1")

  //     f2 <- step(f1, middleL, _.middle.forward)
  //     _   = println(s"f2 = $f2")

  //     f3 <- step(f2, leftL, _.left.forward)
  //     _   = println(s"f3 = $f3")
  //   } yield f3

  // private[enigma] def reverse(r0: Char): State[Machine, Char] =
  //   for {
  //     r1 <- step(r0, leftL, _.left.forward)
  //     _   = println(s"r1 = $r1")

  //     r2 <- step(r1, middleL, _.middle.forward)
  //     _   = println(s"r2 = $r2")

  //     r3 <- step(r2, rightL, _.right.reverse)
  //     _   = println(s"r3 = $r3")
  //   } yield r3

  // def scramble(c0: Char): State[Machine,Char] =
  //   for {
  //     a <- forward(c0)
  //     _  = println("1 ::: " + a)

  //     m <- get[Machine]
  //     b <- state(m.reflector.transform(a))
  //     _  = println("2 ::: " + b)

  //     c <- reverse(b)
  //     _  = println("3 ::: " + c)
  //   } yield c

  // syntax for state monad application
  // implicit class...
  // def use(c: Char): Char =
    // Machine.use(c).eval(this)

  // def use(c0: Char): State[Machine, Char] =
  //   for {
  //     m <- get[Machine]
  //     a <- state(m.plugboard.transform(c0))
  //     b <- scramble(a)
  //     c <- state(m.plugboard.transform(b))
  //   } yield c
}
