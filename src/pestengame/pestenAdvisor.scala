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
    * @param knocked The players which have knocked on the table (1 card left).
    *                In a game of four players, including the player themselves, this
    *                array can at most have three entries. A value of 1 would correspond
    *                to the next player, 2 would be the player after, and 3 would be the
    *                last player (and the player which comes right before the current player).
    * @param debt The amount of debt (of twos and jokers). Should be zero if there is no debt.
    * @return The recommended card to use on both the short term and the long term, or Grab, which represents grabbing a new card.
    */
  def adviseMove(topCard: Card, myCards: List[Card], usedCards: List[Card], playerCount: Int, knocked: List[Int], debt: Int): Either[Card, Grab] = {
    hasSpecialCards(myCards) ? adviseComplexMove(topCard, myCards, usedCards, playerCount, knocked, debt) | adviseSimpleMove(topCard, myCards, usedCards, debt)
  }

  def adviseSimpleMove(topCard: Card, myCards: List[Card], usedCards: List[Card], debt: Int): Either[Card, Grab] = {
    // If we have debt, there's nothing we can do without special cards
    if (hasDebt(debt)) return box(grab())

    // Check if we can win
    val winCondition = checkDirectWinCondition(topCard, myCards)
    if (winCondition != null) return box(winCondition)

    // If not, decide on a card based on rarity
    val myFilteredCards = possibleMoves(topCard, myCards)
    myFilteredCards.isEmpty ? box(grab()) | box(determineMostCommonCard(myFilteredCards, usedCards))
  }

  def adviseComplexMove(topCard: Card, myCards: List[Card], usedCards: List[Card], playerCount: Int, knocked: List[Int], debt: Int): Either[Card, Grab] = {
    // If there is debt, handle it accordingly
    if (hasDebt(debt)) return handleDebt(myCards, usedCards, playerCount)

    // Check if we can win
    val winCondition = checkWinConditions(topCard, myCards)
    if (winCondition != null) return box(winCondition)

    // If not, determine the most logical card to use
    null // TODO
  }
}
