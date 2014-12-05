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

  val lenserM = Lenser[Machine]
  val rightL = lenserM(_.right)
  val middleL = lenserM(_.middle)
  val leftL = lenserM(_.left)

  val lenserR = Lenser[Rotor]
  val rotorL: Lens[Rotor,Rotor,Char,Char] = lenserR(_.position)

  private[enigma] def rtl(m: Machine)(c: Char): Char =
    m.right.forward _ andThen m.middle.forward andThen m.left.forward apply(c)

  private[enigma] def ltr(m: Machine)(c: Char): Char =
    m.left.reverse _ andThen m.middle.reverse andThen m.right.reverse apply(c)

  def run(c: Char): State[Machine, Char] = {
    def stepRotor(r: Rotor): Rotor =
      rotorL.modify(r, Alphabet.nextLetter)

    def right(m: Machine): Machine =
      m |-> rightL modify(stepRotor)

    def middle(m: Machine): Machine =
      m |-> middleL modify(r =>
        if(m.right.notch == r.position || m.left.notch == r.position) stepRotor(r)
        else r)

    def left(m: Machine): Machine =
      m |-> leftL modify(r =>
        if(r.position == m.middle.notch) stepRotor(r)
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

  // syntax for state monad application
  implicit class UseSyntax(m: Machine){
    import scalaz.std.vector._
    import scalaz.syntax.traverse._
    def use(c: Char*): String =
      c.toVector
        .map(Character.toUpperCase)
        .filter(x => Alphabet.ordered.contains(x))
        .traverseU(run)
        .map(_.mkString.grouped(4).mkString(" "))
        .eval(m)
  }
}
