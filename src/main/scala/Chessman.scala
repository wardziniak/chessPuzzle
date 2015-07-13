/**
 * Created by bartek on 7/13/15.
 */
trait Chessman {
  def beats(field: Field)

  def field: Field
}

case class Queen(field: Field) extends Chessman {
  override def beats(other: Field): Unit = field.sameDiagonals(other) || field.sameOrthogonal(other)
}


case class Field(x: Int, y: Int) {
  def sameDiagonals(other: Field): Boolean = Math.abs(x - other.x) == Math.abs(y - other.y)
  def sameOrthogonal(other: Field): Boolean = x == other.x || y == other.y
}
