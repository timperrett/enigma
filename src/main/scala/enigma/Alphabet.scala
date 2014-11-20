package enigma

// some convenience as we are primarily working with letters A through Z.
object Alphabet {
  // a single sequence of A to Z letters, 26 chars long
  val ordered: Seq[Char] = ('A' to 'Z').toList

  // compute length of the alphabet so that the code reads nicer and
  // we're not doing .length all over the place. For syntax only.
  val length: Int = ordered.length

  // produce a randomly shuffled sequence of the alphabet.
  def shuffled: Seq[Char] =
    util.Random.shuffle(ordered).toList

  def empty: Seq[Char] = Seq.empty

  def nextLetter(after: Char): Char = {
    if(after == 'Z') 'A'
    else (after+1).toChar
  }

  // provide an infinite stream of alphabet letters that loops
  // around A -> Z -> A -> Z etc
  val stream: Stream[Char] = {
    def go(c: Char): Stream[Char] =
      c #:: go(Alphabet.nextLetter(c))
    go('A')
  }
}
