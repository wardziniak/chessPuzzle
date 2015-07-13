/**
 * Created by bartek on 7/13/15.
 */
case class ChessBoard(size: Int, free: List[Field]) {
  def this(size: Int) = {
    this(size, List.tabulate(size* size)(n => new Field(n/size, n % size)))
  }
}

object ChessBoard {
  //def apply(size: Int, free: List[Field]) = new ChessBoard(size, free)
}
