package pestengame

import pestengame.model.Card
import pestengame.utility.$._

object main {
  def main(args: Array[String]) : Unit = {
    val topCard = new Card(7, 'D')
    val myCards = List(
      new Card(7, 'H'),
      new Card(7, 'D'),
      new Card(7, 'S'),
      new Card(7, 'C'),
      new Card(4, 'S'),
      new Card(2, 'S')
    )
    val usedCards = List(new Card(4, 'C'))
    val playerCount = 4
    val knocked = List[Int]()
    val debt = 2

    val x = pestenAdvisor.adviseMove(topCard, myCards, usedCards, playerCount, knocked, debt)
    println(x.left)
    println(x.right)


    val tmp = List(1, 2, 3)
  }
}
