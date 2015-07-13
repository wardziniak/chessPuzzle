/**
 * Created by bartek on 7/13/15.
 */
object Algorithm {

  def solution(size: Int, chessmen: List[UnsetChessman]) {

  }

  def countSolution(size: Int, chessmanList: List[Chessman]) = {
    //chessmanList.permutations.collect()
  }

  def singlePermutationSolution(freeFields: List[Field], chessmen: List[UnsetChessman], alreadySetChessmen: List[Chessman]): Int = {
    chessmen match {
      case Nil => {
        //println(s"Nil ${freeFields.size}")
        0
      }
      case currentPiece :: Nil =>
        //println(s"h :: Nil ${freeFields.size}")
        freeFields.filterNot(p => currentPiece.setOnField(p).beatsChessmen(alreadySetChessmen)).size
        //freeFields.size
      case (h :: t) => {
        val currentPiece = h
        val restPieces = t
        freeFields.filterNot(p => currentPiece.setOnField(p).beatsChessmen(alreadySetChessmen)).collect { case p =>
          val setChessmen = currentPiece.setOnField(p)
          val toCheck = freeFields.filter(q => q.isAfter(p) && !setChessmen.beats(q))

          if (toCheck.nonEmpty)
            singlePermutationSolution(toCheck, t, setChessmen :: alreadySetChessmen)
          else 0
        }.sum


//        freeFields.collect { case p =>
//          //val unAttacked = freeFields.fil
//          val setChessmen = currentPiece.setOnField(p)
//          val toCheck = freeFields.filter(q => q.isAfter(p) && !setChessmen.beats(q))
//          //println(p)
//          //println(toCheck)
//          if (toCheck.nonEmpty)
//            singlePermutationSolution(toCheck, t, setChessmen :: alreadySetChessmen)
//          else 0
//        }.sum
      }
    }
  }

}
