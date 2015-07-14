/**
 * Created by bartek on 7/13/15.
 * File with Chessmen definition
 */

case class Chessman (field: Field, checkingFun: (Field, Field) => Boolean) {
  def beats(other: Field) = checkingFun(field, other)
  def beatsChessmen(chessmen: List[Chessman]) = chessmen.find(p => this.beats(p.field)) match {
    case None => false
    case _ => true
  }
}

object Chessman {
  private val chess = Map (
    'Q' -> new UnsetQueen,
    'K' -> new UnsetKing,
    'B' -> new UnsetBishop,
    'R' -> new UnsetRook,
    'S' -> new UnsetKnight
  )

  def apply(sign: Char) = chess.get(sign.toUpper)
}

trait UnsetChessman {
  def checkingFunction: ((Field, Field) => Boolean)
  def setOnField(field: Field) = new Chessman(field, checkingFunction)
}

class UnsetQueen extends UnsetChessman {
  override def checkingFunction: (Field, Field) => Boolean = (field1, field2) => field1.sameDiagonals(field2) || field1.sameOrthogonal(field2)
}

class UnsetBishop extends UnsetChessman {
  override def checkingFunction: (Field, Field) => Boolean = (field1, field2) => field1.sameDiagonals(field2)
}

class UnsetRook extends UnsetChessman {
  override def checkingFunction: (Field, Field) => Boolean = (field1, field2) => field1.sameOrthogonal(field2)
}

class UnsetKing extends UnsetChessman {
  override def checkingFunction: (Field, Field) => Boolean = (field1, field2) => field1.kingBeats(field2)
}

class UnsetKnight extends UnsetChessman {
  override def checkingFunction: (Field, Field) => Boolean = (field1, field2) => field1.canJumpTo(field2)
}

