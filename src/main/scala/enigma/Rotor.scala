package enigma

case class Rotor(
  wiring: String,  // the mapping of normal A->Z letters to its scrambled form
  ring: Char = 'A', // Ringstellung: posistion of the wiring relative to the offset
  notch: Char,
  posistion: Char // Grundstellung the posistion the alphabet ring is currently rotated too
){
  val forwardMapping: Seq[Char] = wiring.toSeq
  val reverseMapping: Seq[Char] = wiring.reverse.toSeq
  val ringAsInt: Int = ring + 'A'
  val posistionAsInt: Int = 'A' + posistion
  val offset: Int = posistionAsInt - ringAsInt
  val size = wiring.length

  private def adjust(c: Char): Int =
    (size + (c - 'A') + offset) % size

  private def encode(c: Char, w: Seq[Char]): Char = {
    val adjustment = adjust(c)
    val resultOffset = (size + forwardMapping(adjustment) - 'A' - offset) % size
    ('A' + resultOffset).toChar
  }

  def forward(c: Char): Char =
    encode(c, forwardMapping)

  def reverse(c: Char): Char =
    encode(c, reverseMapping)

  def hasReachedNotch: Boolean =
    notch == posistion
}

// Actual configurations used by the Nazi's both before and during the war
// from the Enigma I & M3 army/navy machines:
// http://en.wikipedia.org/wiki/Enigma_rotor_details#Rotor_wiring_tables
object Rotors {
  def I(p: Char) = Rotor(
    wiring  = "EKMFLGDQVZNTOWYHXUSPAIBRCJ",
    notch  = 'Q',
    ring   = 'A',
    posistion = p
  )
  def II(p: Char) = Rotor(
    wiring  = "AJDKSIRUXBLHWTMCQGZNPYFVOE",
    notch  = 'E',
    ring   = 'A',
    posistion = p
  )
  def III(p: Char) = Rotor(
    wiring  = "BDFHJLCPRTXVZNYEIWGAKMUSQO",
    notch  = 'V',
    ring   = 'A',
    posistion = p
  )
  def IV(p: Char) = Rotor(
    wiring  = "ESOVPZJAYQUIRHXLNFTGKDCMWB",
    notch  = 'J',
    ring   = 'A',
    posistion = p
  )
  def V(p: Char) = Rotor(
    wiring  = "VZBRGITYUPSDNHLXAWMJQOFECK",
    notch  = 'Z',
    ring   = 'A',
    posistion = p
  )
}
