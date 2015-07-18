/**
 * Created by wardziniak on 7/13/15.
 * File with Chessboard definition
 */

object ChessBoard {
  def createChessBoard(size: Int) = List.tabulate(size* size)(n => Field(n/size, n % size))
}

case class Field(x: Int, y: Int) {
  def isQueueBeats(other: Field): Boolean = {
    val xDiff = Math.abs(x - other.x)
    val yDiff = Math.abs(y - other.y)
    xDiff == 0 || yDiff == 0 || xDiff == yDiff
  }
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

  def isBeatenByOneOfChessPieces(chessmen: List[Chessman]) = chessmen.find(p => p.beats(this)) match {
    case None => false
    case Some(_) => true
  }

  def beatsChessPieces(beatFunction: (Field, Field) => Boolean, chessmen: List[Chessman]): Boolean =
    chessmen.find(p => beatFunction(this, p.field)) match {
      case None => false
      case _ => true
    }
}
