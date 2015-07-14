import scala.annotation.tailrec

/**
 * Created by bartek on 7/13/15.
 * Algorithm implementation
 */
object Algorithm {

  @tailrec
  def numberOfAllSolutionsForFirstChessman(freeFields: List[Field], chessmen: List[UnsetChessman], alreadySetChessmen: List[Chessman], result: Int): Int = freeFields match {
    case Nil => result
    case h :: t =>
      val setChessman = chessmen.head.setOnField(h)
      if (!setChessman.beatsChessmen(alreadySetChessmen)) {
        val toCheck = t.filter(q => !setChessman.beats(q))
        if (toCheck.nonEmpty) numberOfAllSolutionsForFirstChessman(t, chessmen, alreadySetChessmen,
          result +numberOfSinglePermutationSolution(toCheck, chessmen.tail, setChessman :: alreadySetChessmen))
        else
          numberOfAllSolutionsForFirstChessman(t, chessmen, alreadySetChessmen, result)
      }
      else
        numberOfAllSolutionsForFirstChessman(t, chessmen, alreadySetChessmen, result)
  }

  def numberOfSinglePermutationSolution(freeFields: List[Field], chessmen: List[UnsetChessman], alreadySetChessmen: List[Chessman]): Int = {
    chessmen match {
      case Nil => 0
      case currentPiece :: Nil =>
        freeFields.filterNot(p => p.beatsChessmen(currentPiece.checkingFunction, alreadySetChessmen)).size
      case (_) =>
        numberOfAllSolutionsForFirstChessman(freeFields, chessmen, alreadySetChessmen, 0)
    }
  }


  @tailrec
  def returnAllSolutionsForFirstChessman(freeFields: List[Field], chessmen: List[UnsetChessman], alreadySetChessmen: List[Chessman], result: List[List[Chessman]]): List[List[Chessman]] = freeFields match {
    case Nil => result
    case h :: t =>
      val setChessman = chessmen.head.setOnField(h)
      if (!setChessman.beatsChessmen(alreadySetChessmen)) {
        val toCheck = t.filter(q => !setChessman.beats(q))
        if (toCheck.nonEmpty) returnAllSolutionsForFirstChessman(t, chessmen, alreadySetChessmen, result ++ returnSolutionsForSinglePermutation(toCheck, chessmen.tail, setChessman :: alreadySetChessmen))
        else
          returnAllSolutionsForFirstChessman(t, chessmen, alreadySetChessmen, result)
      }
      else
        returnAllSolutionsForFirstChessman(t, chessmen, alreadySetChessmen, result)
  }

  def returnSolutionsForSinglePermutation(freeFields: List[Field], chessmen: List[UnsetChessman], alreadySetChessmen: List[Chessman]): List[List[Chessman]] = {
    chessmen match {
      case Nil => Nil
      case currentPiece :: Nil =>
        freeFields.filterNot(p => p.beatsChessmen(currentPiece.checkingFunction, alreadySetChessmen)).collect { case p =>
          currentPiece.setOnField(p) :: alreadySetChessmen
        }
      case (_) =>
        returnAllSolutionsForFirstChessman(freeFields, chessmen, alreadySetChessmen, Nil)
    }
  }

  def numberOfSinglePermutationSolution1(freeFields: List[Field], chessmen: List[UnsetChessman], alreadySetChessmen: List[Chessman]): Int = {
    (freeFields, chessmen) match {
      case (_, Nil) => 0
      case (h :: t, currentPiece :: Nil) => freeFields.filterNot(p => p.beatsChessmen(currentPiece.checkingFunction, alreadySetChessmen)).size
      case (Nil, _) => 0
      case (_, _) =>
        val setChessman = chessmen.head.setOnField(freeFields.head)
        if (!setChessman.beatsChessmen(alreadySetChessmen)) {
          val toCheck = freeFields.tail.filter(q => !setChessman.beats(q))
          if (toCheck.nonEmpty) numberOfSinglePermutationSolution1(freeFields.tail, chessmen, alreadySetChessmen) +
            numberOfSinglePermutationSolution1(toCheck, chessmen.tail, setChessman :: alreadySetChessmen)
          else
            numberOfSinglePermutationSolution1(freeFields.tail, chessmen, alreadySetChessmen)
        }
        else
          numberOfSinglePermutationSolution1(freeFields.tail, chessmen, alreadySetChessmen)
    }
  }
}
