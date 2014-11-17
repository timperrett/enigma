package enigma

// some convenience as we are primarily working with letters A through Z.
object Alphabet {
  val ordered: Seq[Char] = ('A' to 'Z').toList

  // compute length of the alphabet so that the code reads nicer and
  // we're not doing .length all over the place. For syntax only.
  val length: Int = ordered.length

  // produce a randomly shuffled sequence of the alphabet.
  def shuffled: Seq[Char] =
    util.Random.shuffle(ordered).toList

  def empty: Seq[Char] = Seq.empty

  // imperitive, but it works. probally a better way.
  def nextLetter(after: Char): Char = {
    val i = ordered.indexOf(after)
    if(i == (length-1) || i == -1) ordered.head
    else ordered.apply(i+1)
  }
}
