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

