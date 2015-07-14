import scala.annotation.tailrec

/**
 * Created by bartek on 7/13/15.
 */
object Algorithm {

  @tailrec
  def checkAllSolutionsForFirstChessman(freeFields: List[Field], chessmen: List[UnsetChessman], alreadySetChessmen: List[Chessman], result: Int): Int = freeFields match {
    case Nil => result
    case h :: t =>
      //val tmpVal = checkAllSolutionsForFirstChessman(t, chessmen, alreadySetChessmen)
      val setChessman = chessmen.head.setOnField(h)
      var tmpVal1 = 0
      if (!setChessman.beatsChessmen(alreadySetChessmen)) {
        val toCheck = t.filter(q => !setChessman.beats(q))
        if (toCheck.nonEmpty) checkAllSolutionsForFirstChessman(t, chessmen, alreadySetChessmen, result +singlePermutationSolution(toCheck, chessmen.tail, setChessman :: alreadySetChessmen))
        else
          checkAllSolutionsForFirstChessman(t, chessmen, alreadySetChessmen, result)
      }
      else
        checkAllSolutionsForFirstChessman(t, chessmen, alreadySetChessmen, result)
//      val tmpVal = checkAllSolutionsForFirstChessman(t, chessmen, alreadySetChessmen)
//      val setChessman = chessmen.head.setOnField(h)
//      if (!setChessman.beatsChessmen(alreadySetChessmen)) {
//        val toCheck = t.filter(q => !setChessman.beats(q))
//        if (toCheck.nonEmpty) tmpVal + singlePermutationSolution(toCheck, chessmen.tail, setChessman :: alreadySetChessmen)
//        else tmpVal
//      }
//      else tmpVal
  }

  def singlePermutationSolution(freeFields: List[Field], chessmen: List[UnsetChessman], alreadySetChessmen: List[Chessman]): Int = {
    chessmen match {
      case Nil => 0
      case currentPiece :: Nil =>
        freeFields.filterNot(p => currentPiece.setOnField(p).beatsChessmen(alreadySetChessmen)).size
        //freeFields.filterNot(p => p.beatsChessmen(currentPiece.checkingFunction, alreadySetChessmen)).size
      case (_) =>
//        freeFields.filterNot(p => currentPiece.setOnField(p).beatsChessmen(alreadySetChessmen)).collect { case p =>
//          val setChessmen = currentPiece.setOnField(p)
//          val toCheck = freeFields.filter(q => q.isAfter(p) && !setChessmen.beats(q))
//
//          if (toCheck.nonEmpty)
//            singlePermutationSolution(toCheck, restOfPieces, setChessmen :: alreadySetChessmen)
//          else 0
//        }.sum

        checkAllSolutionsForFirstChessman(freeFields, chessmen, alreadySetChessmen, 0)
    }
  }


  def returnAllSolutionsForFirstChessman(freeFields: List[Field], chessmen: List[UnsetChessman], alreadySetChessmen: List[Chessman]): List[List[Chessman]] = freeFields match {
    case Nil => Nil
    case h :: t =>
      val tmpVal = returnAllSolutionsForFirstChessman(t, chessmen, alreadySetChessmen)
      val setChessman = chessmen.head.setOnField(h)
      if (!setChessman.beatsChessmen(alreadySetChessmen)) {
        val toCheck = t.filter(q => !setChessman.beats(q))
        if (toCheck.nonEmpty) tmpVal ++ returnSolutionsForSinglePermutation(toCheck, chessmen.tail, setChessman :: alreadySetChessmen)
        else tmpVal
      }
      else tmpVal
  }

  def returnSolutionsForSinglePermutation(freeFields: List[Field], chessmen: List[UnsetChessman], alreadySetChessmen: List[Chessman]): List[List[Chessman]] = {
    chessmen match {
      case Nil => Nil
      case currentPiece :: Nil =>
        freeFields.filterNot(p => currentPiece.setOnField(p).beatsChessmen(alreadySetChessmen)).collect { case p =>
          currentPiece.setOnField(p) :: alreadySetChessmen
        }
      //freeFields.filterNot(p => p.beatsChessmen(currentPiece.checkingFunction, alreadySetChessmen)).size
      case (_) =>
        //        freeFields.filterNot(p => currentPiece.setOnField(p).beatsChessmen(alreadySetChessmen)).collect { case p =>
        //          val setChessmen = currentPiece.setOnField(p)
        //          val toCheck = freeFields.filter(q => q.isAfter(p) && !setChessmen.beats(q))
        //
        //          if (toCheck.nonEmpty)
        //            singlePermutationSolution(toCheck, restOfPieces, setChessmen :: alreadySetChessmen)
        //          else 0
        //        }.sum

        returnAllSolutionsForFirstChessman(freeFields, chessmen, alreadySetChessmen)
    }
  }

}
