package enigma

object Main {
  import Rotors._

  val enigma = Machine(
    plugboard = Plugboard(Alphabet.ordered),
    left = I('A'),
    middle = II('A'),
    right = III('A'),
    reflector = Reflectors.B
  )

  def main(args: Array[String]): Unit = {

    println()
    println("Type the text you wish to encrypt.")
    println("Press [Enter] to stop the interactive shell.")
    println()

    Iterator.continually(io.StdIn.readLine)
      .takeWhile(_.nonEmpty)
      .foreach { line =>
        println(enigma.use(line:_*))
        println()
      }
  }
}
