package pestengame

import pestengame.model.Card
import pestengame.utility.$

object main {
  def main(args: Array[String]) : Unit = {
    val topCard = new Card(7, 'D')
    val myCards = List(
      new Card(7, 'H'),
      new Card(7, 'D'),
      new Card(7, 'S'),
      new Card(7, 'C'),
      new Card(4, 'S')
    )
    val usedCards = List(new Card(4, 'C'))
    val playerCount = 4
    val nextKnocked = false
    val prevKnocked = false

    val x = pestenAdvisor.adviseMove(topCard, myCards, usedCards, playerCount, nextKnocked, prevKnocked)
    println(x.left)
    println(x.right)
  }
}
