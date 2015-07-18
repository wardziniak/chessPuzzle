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

  @tailrec
  def singlePermutationNumberOfSolutionTailRec(caseList: List[(List[Field], List[UnsetChessman], List[Chessman])], numberOfAlreadyFindSolution: Int): Int = caseList match {
    case Nil =>
      numberOfAlreadyFindSolution
    case head :: tail =>
      val freeFields = head._1
      val chessmen = head._2
      val alreadySetChessman = head._3
      (freeFields, chessmen) match {
        case (_, Nil) =>
          singlePermutationNumberOfSolutionTailRec(tail, numberOfAlreadyFindSolution)
        case (Nil, _) =>
          singlePermutationNumberOfSolutionTailRec(tail, numberOfAlreadyFindSolution)
        case (h :: t, currentPiece :: Nil) =>
          val tmpNumberSolution = freeFields.count(p => !p.beatsChessmen(currentPiece.checkingFunction, alreadySetChessman))
          singlePermutationNumberOfSolutionTailRec(tail, numberOfAlreadyFindSolution+tmpNumberSolution)
        case (_, _) =>
          val chessManToSet = chessmen.head
          val fieldWhenChessmanCanBeSet = freeFields.filterNot(p => p.beatsChessmen(chessManToSet.checkingFunction, alreadySetChessman))
          val newCaseToCheck = fieldWhenChessmanCanBeSet.collect{ case p =>
            (freeFields.filter(q => q.isAfter(p) && !chessManToSet.setOnField(p).beats(q)),chessmen.tail, chessManToSet.setOnField(p) :: alreadySetChessman)}
          singlePermutationNumberOfSolutionTailRec(newCaseToCheck ++ tail, numberOfAlreadyFindSolution)
      }
  }

  @tailrec
  def singlePermutationSolutionTailRec(caseList: List[(List[Field], List[UnsetChessman], List[Chessman])], alreadyFoundSolution: List[List[Chessman]]): List[List[Chessman]] = caseList match {
    case Nil =>
      alreadyFoundSolution
    case head :: tail =>
      val freeFields = head._1
      val chessmen = head._2
      val alreadySetChessman = head._3
      (freeFields, chessmen) match {
        case (_, Nil) =>
          singlePermutationSolutionTailRec(tail, alreadyFoundSolution)
        case (Nil, _) =>
          singlePermutationSolutionTailRec(tail, alreadyFoundSolution)
        case (h :: t, currentPiece :: Nil) =>
          val newSolutions = freeFields.filter(p => !p.beatsChessmen(currentPiece.checkingFunction, alreadySetChessman)).collect {
            case p => chessmen.head.setOnField(p) :: alreadySetChessman
          }
          singlePermutationSolutionTailRec(tail, alreadyFoundSolution ++ newSolutions)
        case (_, _) =>
          val chessManToSet = chessmen.head
          val fieldWhenChessmanCanBeSet = freeFields.filterNot(p => p.beatsChessmen(chessManToSet.checkingFunction, alreadySetChessman))
          val newCaseToCheck = fieldWhenChessmanCanBeSet.collect{ case p =>
            (freeFields.filter(q => q.isAfter(p) && !chessManToSet.setOnField(p).beats(q)),chessmen.tail, chessManToSet.setOnField(p) :: alreadySetChessman)}
          singlePermutationSolutionTailRec(newCaseToCheck ++ tail, alreadyFoundSolution)
      }
  }


}
