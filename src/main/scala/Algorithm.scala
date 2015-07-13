/**
 * Created by bartek on 7/13/15.
 */
object Algorithm {

  def solution(size: Int, chessmen: List[UnsetChessman]) {

  }

  def countSolution(size: Int, chessmanList: List[Chessman]) = {
    //chessmanList.permutations.collect()
  }

  def singlePermutationSolution(freeFields: List[Field], chessmen: List[UnsetChessman]): Int = {
    chessmen match {
      case Nil => {
        println(s"Nil ${freeFields.size}")
        0
      }
      case (h :: t) => {
        val currentPiece = h
        val restPieces = t
        println(s"More $currentPiece")
        if (t == Nil) {
          println( freeFields.size)
          freeFields.size
        }
        else
          freeFields.collect { case p =>
            //val unAttacked = freeFields.fil
            val toCheck = freeFields.filter(q => q.isAfter(p) && !currentPiece.setOnField(p).beats(q))
            if (toCheck.nonEmpty)
              singlePermutationSolution(toCheck, restPieces)
            else 0
          }.sum
      }
    }
  }

}
