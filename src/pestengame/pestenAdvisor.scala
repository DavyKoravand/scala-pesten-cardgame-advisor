package pestengame

import pestengame.model.{Card, Grab}
import pestenHelper._
import pestengame.utility.$._
import pestengame.model.Ternary._

object pestenAdvisor {
  /**
    * Advise a move based on the current game state.
    * @param topCard The visible top card on the stack.
    * @param myCards The cards in the hand of the user.
    * @param usedCards The known cards that have already been used, if any.
    * @param playerCount The amount of players participating.
    * @param nextKnocked Whether the next player has knocked on the table (has 1 card left).
    * @param prevKnocked Whether the previous player has knocked on the table (has 1 card left).
    * @return The recommended card to use on both the short term and the long term, or Grab, which represents grabbing a new card.
    */
  def adviseMove(topCard: Card, myCards: List[Card], usedCards: List[Card], playerCount: Int, nextKnocked: Boolean, prevKnocked: Boolean): Either[Card, Grab] = {
    hasSpecialCards(myCards) ? adviseComplexMove(topCard, myCards, usedCards, playerCount, nextKnocked, prevKnocked) | adviseSimpleMove(topCard, myCards, usedCards)
  }

  def adviseSimpleMove(topCard: Card, myCards: List[Card], usedCards: List[Card]): Either[Card, Grab] = {
    // Check if we can win
    val winCondition = checkDirectWinCondition(topCard, myCards)
    if (winCondition != null) return box(winCondition)

    // If not, decide on a card based on rarity
    val myFilteredCards = possibleMoves(topCard, myCards)
    myFilteredCards.isEmpty ? box(grab()) | box(determineMostCommonCard(myFilteredCards, usedCards))
  }

  def adviseComplexMove(topCard: Card, myCards: List[Card], usedCards: List[Card], playerCount: Int, nextKnocked: Boolean, prevKnocked: Boolean): Either[Card, Grab] = {
    // Check if we can win
    val winCondition = checkWinConditions(topCard, myCards)
    if (winCondition != null) return box(winCondition)

    // If not, determine the most logical card to use
    // TODO
    box(grab())
  }
}
