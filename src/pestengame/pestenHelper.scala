package pestengame

import pestengame.model.{Card, Grab}
import pestengame.model.Ternary._
import pestengame.utility.$._

object pestenHelper {
  def hasSpecialCards(cards: List[Card]): Boolean = {
    cards.exists(card => isSpecialCard(card))
  }

  def isSpecialCard(card: Card): Boolean = {
    val specialCards = Map(card.A() -> true, 2 -> true, 7 -> true, 8 -> true, card.J() -> true, card.Q() -> true, card.K() -> true, card.Joker() -> true)

    specialCards.contains(card.value())
  }

  def determineMostCommonCard(myPossibleCards: List[Card], usedCards: List[Card]) : Card = {
    var cardUsageMap = myPossibleCards.map(card => (card, 0)).toMap

    for (myCard <- myPossibleCards)
      for (usedCard <- usedCards)
        callIf(moveValid(usedCard, myCard), () => cardUsageMap = cardUsageMap.updated(myCard, cardUsageMap(myCard)+1))

    cardUsageMap.maxBy{case (_, count) => count}._1
  }

  def moveValid(topCard: Card, myCard: Card): Boolean = {
    mayAlwaysBePlaced(myCard) || valueEquals(myCard, topCard) || symbolEquals(myCard, topCard)
  }

  def mayAlwaysBePlaced(card: Card): Boolean = {
    hasValue(card, card.J()) || hasValue(card, card.Joker())
  }

  def hasSymbol(card: Card, symbol: Int): Boolean = {
    card.symbol() == symbol
  }

  def hasValue(card: Card, value: Int): Boolean = {
    card.value() == value
  }

  def valueEquals(cardA: Card, cardB: Card): Boolean = {
    cardA.value() == cardB.value()
  }

  def symbolEquals(cardA: Card, cardB: Card): Boolean = {
    cardA.symbol() == cardB.symbol()
  }

  def possibleMoves(topCard: Card, myCards: List[Card]) : List[Card] = {
    (myCards.length == 1 && isSpecialCard(myCards.head)) ? List[Card]() | myCards.filter(moveValid(topCard, _))
  }

  def box(card: Card): Either[Card, Grab] = {
    Left(card)
  }

  def box(grab: Grab): Either[Card, Grab] = {
    Right(grab)
  }

  def box(none: Null): Either[Card, Grab] = {
    null
  }

  def box(either: Either[Card, Grab]): Either[Card, Grab] = {
    either
  }

  def grab(reason: String): Grab = {
    new Grab(reason)
  }

  def checkWinConditions(topCard: Card, myCards: List[Card]): Card = {
    oneOf(checkDirectWinCondition(topCard, myCards), checkChainingWinCondition(topCard, myCards))
  }

  def checkDirectWinCondition(topCard: Card, myCards: List[Card]): Card = {
    (myCards.length == 1 && moveValid(topCard, myCards.head) && !isSpecialCard(myCards.head)) ? topCard | null
  }

  def checkChainingWinCondition(topCard: Card, myCards: List[Card]): Card = {
    val sevens = filterCardsByValue(myCards, 7)

    // Fail if there are no sevens
    if (sevens.isEmpty)
      return null

    // Fail if there is more than one non-seven card
    if ((myCards.length - sevens.length) != 1)
      return null

    // If we came this far, there is at least one seven and exactly one non-seven card
    val finishingCard = myCards.diff(sevens).head

    // Cannot finish with a special card
    if (isSpecialCard(finishingCard))
      return null

    // Check if the sevens and the remaining card can finish
    for (sevenFirst <- sevens)
      if (moveValid(topCard, sevenFirst))
        for (sevenSecond <- sevens)
          if (moveValid(sevenSecond, finishingCard))
            return sevenFirst

    // Failed, we cannot finish with these cards
    null
  }

  def filterCardsByValue(cards: List[Card], value: Int): List[Card] = {
    cards.filter(card => card.value() == value)
  }

  def filterCardsBySymbol(cards: List[Card], symbol: Char): List[Card] = {
    cards.filter(card => card.symbol() == symbol)
  }

  def hasDebt(debt: Int): Boolean = {
    debt > 0
  }

  def handleDebt(myCards: List[Card], usedCards: List[Card], playerCount: Int, debt: Int): Either[Card, Grab] = {
    // Find all twos and jokers
    val debtCards = findDebtCards(myCards)
    if (debtCards.isEmpty) return box(grab("There is a debt, but we have no 2s or jokers."))

    // If we have five players, we should just place a card, since we most likely won't be chained
    // If the debt has surpassed five, we should also just place a card to avoid having to pay it all
    if (playerCount >= 5 || debt > 5) return box(lowestDebtCard(debtCards))

    // Determine the best card based on the probability of being chained
    minus(7, len(findDebtCards(usedCards)), len(debtCards)) match {
      case x if minus(x, playerCount) > 1 && len(debtCards) == 1 => box(grab("While we do have a debt card, there is a reasonable chance that it will be chained back to us."))
      case _ => box(lowestDebtCard(debtCards))
    }
  }

  def findDebtCards(cards: List[Card]): List[Card] = {
    filterCardsByValue(cards, 2).union(filterCardsByValue(cards, 14))
  }

  def lowestDebtCard(debtCards: List[Card]): Card = {
    for (card <- debtCards)
      if (card.value() == 2) return card

    random(debtCards)
  }

  def highestDebtCard(debtCards: List[Card]): Card = {
    for (card <- debtCards)
      if (card.value() == card.Joker) return card

    random(debtCards)
  }

  def determineLogicDebt(topCard: Card, myCards: List[Card], knocked: List[Int]): Card = {
    val validDebtCards = possibleMoves(topCard, findDebtCards(myCards))
    if (validDebtCards.nonEmpty && knocked.contains(1)) lowestDebtCard(validDebtCards) else null
  }

  def determineLogicA(topCard: Card, myCards: List[Card], playerCount: Int, knocked: List[Int]): Card ={
    val validACards = possibleMoves(topCard, filterCardsByValue(myCards, 1))
    if (validACards.nonEmpty && knocked.contains(minus(playerCount, 1))) random(validACards) else null
  }

  def determineLogicEight(topCard: Card, myCards: List[Card], knocked: List[Int]): Card = {
    val validEightCards = possibleMoves(topCard, filterCardsByValue(myCards, 8))
    if (validEightCards.nonEmpty && knocked.contains(1)) random(validEightCards) else null
  }

  def determineLogicJ(): Card = {
null
  }
}
