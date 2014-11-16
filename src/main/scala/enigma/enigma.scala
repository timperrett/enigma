package enigma

object Alphabet {
  val ordered: Seq[Char] = ('A' to 'Z').toList
  def shuffled: Seq[Char] = util.Random.shuffle(ordered).toList
}

case class Plugboard(input: Seq[Char], output: Seq[Char])

object main extends App {
  val p = Plugboard(input = Alphabet.ordered, output = Alphabet.shuffled)

  println(s">>>>> $p")

}