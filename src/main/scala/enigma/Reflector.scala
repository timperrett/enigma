package enigma

// primary difference here is that `Reflector` has no notch, and is always configured
// with symetric wiring tables. i.e. A -> Z, Z -> A. Reflectors also do not have
// configurable rings; they were fixed and came in preset variants: A, B and C
// Known as Umkehrwalze in german.
case class Reflector(wiring: String){
  val mapping: Map[Char,Char] =
    wiring.zipWithIndex.map { case (c,i) => Alphabet.ordered(i) -> c }.toMap

  def transform(c: Char): Char =
    mapping(c) // throws an exception if invalid character.
}

object Reflectors {
  val A = Reflector("EJMZALYXVBWFCRQUONTSPIKHGD")
  val B = Reflector("YRUHQSLDPXNGOKMIEBFZCWVJAT")
  val C = Reflector("FVPJIAOYEDRZXWGCTKUQSBNMHL")
}
