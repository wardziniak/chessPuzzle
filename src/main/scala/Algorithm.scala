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
        //println(s"Nil ${freeFields.size}")
        0
      }
      case current :: Nil =>
        //println(s"h :: Nil ${freeFields.size}")
        freeFields.size
      case (h :: t) => {
        val currentPiece = h
        val restPieces = t
        //println(s"More $currentPiece")
        freeFields.collect { case p =>
          //val unAttacked = freeFields.fil
          val toCheck = freeFields.filter(q => q.isAfter(p) && !currentPiece.setOnField(p).beats(q))
          //println(p)
          //println(toCheck)
          if (toCheck.nonEmpty)
            singlePermutationSolution(toCheck, t)
          else 0
        }.sum
      }
    }
  }

}
