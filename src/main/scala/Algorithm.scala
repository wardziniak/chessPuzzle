/**
 * Created by bartek on 7/13/15.
 */
object Algorithm {

  def solution(size: Int, chessmen: List[UnsetChessman]) {

  }

  def countSolution(size: Int, chessmanList: List[Chessman]) = {
    //chessmanList.permutations.collect()
  }

  def fun(freeFields: List[Field], chessmen: List[UnsetChessman], alreadySetChessmen: List[Chessman]): Int = freeFields match {
    case Nil => 0
    case h :: t => {
      val tmpVal = fun(t, chessmen, alreadySetChessmen)
      val setChessman = chessmen.head.setOnField(h)
      if (!setChessman.beatsChessmen(alreadySetChessmen)) {
        val toCheck = t.filter(q => !setChessman.beats(q))
        if (toCheck.nonEmpty) tmpVal + singlePermutationSolution(toCheck, chessmen.tail, setChessman :: alreadySetChessmen)
        else tmpVal
      }
      else tmpVal
    }
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
      case (currentPiece :: restOfPieces) => {
//        freeFields.filterNot(p => currentPiece.setOnField(p).beatsChessmen(alreadySetChessmen)).collect { case p =>
//          val setChessmen = currentPiece.setOnField(p)
//          val toCheck = freeFields.filter(q => q.isAfter(p) && !setChessmen.beats(q))
//
//          if (toCheck.nonEmpty)
//            singlePermutationSolution(toCheck, restOfPieces, setChessmen :: alreadySetChessmen)
//          else 0
//        }.sum

        fun(freeFields, chessmen, alreadySetChessmen)

//        freeFields.filterNot(p => currentPiece.setOnField(p).beatsChessmen(alreadySetChessmen)).collect { case p =>
//          val setChessmen = currentPiece.setOnField(p)
//          val toCheck = freeFields.filter(q => q.isAfter(p) && !setChessmen.beats(q))
//
//          if (toCheck.nonEmpty)
//            singlePermutationSolution(toCheck, restOfPieces, setChessmen :: alreadySetChessmen)
//          else 0
//        }.sum

//        freeFields.collect { case p =>
//          val setChessmen = currentPiece.setOnField(p)
//          if (currentPiece.setOnField(p).beatsChessmen(alreadySetChessmen)) 0
//          else {
//            val toCheck = freeFields.filter(q => q.isAfter(p) && !setChessmen.beats(q))
//
//            if (toCheck.nonEmpty)
//              singlePermutationSolution(toCheck, restOfPieces, setChessmen :: alreadySetChessmen)
//            else 0
//          }
//        }.sum


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
