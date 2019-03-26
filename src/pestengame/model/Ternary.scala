package pestengame.model

sealed trait TernaryResult[T] extends Any {
  def |(op3: => T): T
}

class Ternary2ndOperand[T](val op2: T) extends AnyVal with TernaryResult[T] {
  def |(op3: => T) = op2
}

class Ternary3rdOperand[T](val op2: T) extends AnyVal with TernaryResult[T] {
  def |(op3: => T) = op3
}

class Ternary(val op1:Boolean) extends AnyVal {
  def ?[A](op2: => A): TernaryResult[A] = if (op1) new Ternary2ndOperand(op2) else new Ternary3rdOperand(op2)
}

object Ternary {
  implicit def toTernary(condition: Boolean) = new Ternary(condition)
}
