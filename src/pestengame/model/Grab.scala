package pestengame.model

class Grab(reason: String) {
  override def toString: String = {
    "Grab a new card from the stack. Reason: " + reason
  }
}
