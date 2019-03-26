package pestengame.model

/**
  * A representation of a card.
  *
  * Card value map:
  * Icon|Num
  * A|1
  * 2|2
  * 3|3
  * 4|4
  * 5|5
  * 6|6
  * 7|7
  * 8|8
  * 9|9
  * 10|10
  * J|11
  * Q|12
  * K|13
  * Joker|14
  *
  * Card symbol map:
  * Symbol|Char
  * Spades|S
  * Diamonds|D
  * Hearts|H
  * Clubs|C
  * Joker|J
  *
  * @param cardValue A number from 0-13.
  * @param cardSymbol A character representing the symbol.
  */
class Card(cardValue: Int, cardSymbol: Char) {
  def value(): Int = cardValue

  def symbol(): Int = cardSymbol

  def A(): Int = 1
  def J(): Int = 11
  def Q(): Int = 12
  def K(): Int = 13
  def Joker(): Int = 14

  override def toString: String = {
    s"[Card] " + valueName(cardValue) + symbolName(cardSymbol)
  }

  def valueName(value: Int): String = {
    value match {
      case 1 => "A"
      case 11 => "J"
      case 12 => "Q"
      case 13 => "K"
      case 14 => ""
      case _ => value.toString
    }
  }

  def symbolName(symbol: Char): String = {
    symbol match {
      case 'D' => "♦"
      case 'C' => "♣"
      case 'S' => "♠"
      case 'H' => "♥"
      case 'J' => "Joker"
      case _ => "?"
    }
  }
}
