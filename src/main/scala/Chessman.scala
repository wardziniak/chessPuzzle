/**
 * Created by bartek on 7/13/15.
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


case class Field(x: Int, y: Int) {
  def sameDiagonals(other: Field): Boolean = Math.abs(x - other.x) == Math.abs(y - other.y)
  def sameOrthogonal(other: Field): Boolean = x == other.x || y == other.y
  def canJumpTo(other: Field): Boolean = {
    val xDiff = Math.abs(x - other.x)
    val yDiff = Math.abs(y - other.y)
    (xDiff == 1 && yDiff == 2) || (xDiff == 2 && yDiff == 1)
  }
  def kingBeats(other: Field) = Math.abs(x - other.x) <= 1 && Math.abs(y - other.y) <= 1
  def isAfter(field: Field) = {
    if (x > field.x) true
    else x == field.x && y > field.y
  }

  def isBeatenByOneOfChessmen(chessmen: List[Chessman]) = chessmen.find(p => p.beats(this)) match {
    case None => false
    case Some(_) => true
  }
}

object Field {
  //def apply(x: Int, y: Int) = new Field(x, y)
}




