package enigma

case class Rotor(
  wiring: String,  // the mapping of normal A->Z letters to its scrambled form
  ring: Char = 'A', // Ringstellung: offset of the wiring relative to the posistion
  notch: Char,
  position: Char // Grundstellung: the position the alphabet ring is currently rotated too
){
  val chars: Seq[Char] = wiring.toUpperCase.toSeq

  val forwardMapping: Seq[Char] = chars
  val reverseMapping: Seq[Char] = wiring.toSeq.zipWithIndex.map {
    case (a,i) => (chars(i) - 'A') -> ('A' + i).toChar
  }.sortBy(_._1).map(_._2).toSeq

  val ringAsInt: Int = ring + 'A'
  val posistionAsInt: Int = 'A' + position
  val offset: Int = posistionAsInt - ringAsInt
  val size = wiring.length

  private def adjust(c: Char): Int =
    (size + (c - 'A') + offset) % size

  private def encode(c: Char, w: Seq[Char]): Char = {
    // println(s">=== $c ===<")
    val adjustment = adjust(c)
    val resultOffset = (size + w(adjustment) - 'A' - offset) % size
    val output = ('A' + resultOffset).toChar
    // println(s"<=== $output ===>")
    output
  }

  def forward(c: Char): Char =
    encode(c, forwardMapping)

  def reverse(c: Char): Char =
    encode(c, reverseMapping)
}

// Actual configurations used by the Nazi's both before and during the war
// from the Enigma I & M3 army/navy machines:
// http://en.wikipedia.org/wiki/Enigma_rotor_details#Rotor_wiring_tables
object Rotors {
  def I(p: Char) = Rotor(
    wiring  = "EKMFLGDQVZNTOWYHXUSPAIBRCJ",
    notch  = 'R',
    ring   = 'A',
    position = p
  )
  def II(p: Char) = Rotor(
    wiring  = "AJDKSIRUXBLHWTMCQGZNPYFVOE",
    notch  = 'F',
    ring   = 'A',
    position = p
  )
  def III(p: Char) = Rotor(
    wiring  = "BDFHJLCPRTXVZNYEIWGAKMUSQO",
    notch  = 'W',
    ring   = 'A',
    position = p
  )
  def IV(p: Char) = Rotor(
    wiring  = "ESOVPZJAYQUIRHXLNFTGKDCMWB",
    notch  = 'K',
    ring   = 'A',
    position = p
  )
  def V(p: Char) = Rotor(
    wiring  = "VZBRGITYUPSDNHLXAWMJQOFECK",
    notch  = 'A',
    ring   = 'A',
    position = p
  )
}
