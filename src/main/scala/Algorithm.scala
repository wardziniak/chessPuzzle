import scala.annotation.tailrec

/**
 * Created by bartek on 7/13/15.
 * Algorithm implementation
 */
object Algorithm {

  @tailrec
  def numberOfAllSolutionsForFirstChessman(freeFields: List[Field], unsetChessPieces: List[UnsetChessman], alreadySetChessPieces: List[Chessman], result: Int): Int = freeFields match {
    case Nil => result
    case h :: t =>
      val setChessman = unsetChessPieces.head.setOnField(h)
      if (!setChessman.beatsChessPieces(alreadySetChessPieces)) {
        val toCheck = t.filter(q => !setChessman.beats(q))
        if (toCheck.nonEmpty) numberOfAllSolutionsForFirstChessman(t, unsetChessPieces, alreadySetChessPieces,
          result +numberOfSinglePermutationSolution(toCheck, unsetChessPieces.tail, setChessman :: alreadySetChessPieces))
        else
          numberOfAllSolutionsForFirstChessman(t, unsetChessPieces, alreadySetChessPieces, result)
      }
      else
        numberOfAllSolutionsForFirstChessman(t, unsetChessPieces, alreadySetChessPieces, result)
  }

  def numberOfSinglePermutationSolution(freeFields: List[Field], unsetChessPieces: List[UnsetChessman], alreadySetChessPieces: List[Chessman]): Int = {
    unsetChessPieces match {
      case Nil => 0
      case currentPiece :: Nil =>
        freeFields.filterNot(p => p.beatsChessPieces(currentPiece.checkingFunction, alreadySetChessPieces)).size
      case (_) =>
        numberOfAllSolutionsForFirstChessman(freeFields, unsetChessPieces, alreadySetChessPieces, 0)
    }
  }


  @tailrec
  def returnAllSolutionsForFirstChessman(freeFields: List[Field], unsetChessPieces: List[UnsetChessman], alreadySetChessPieces: List[Chessman], result: List[List[Chessman]]): List[List[Chessman]] = freeFields match {
    case Nil => result
    case h :: t =>
      val setChessman = unsetChessPieces.head.setOnField(h)
      if (!setChessman.beatsChessPieces(alreadySetChessPieces)) {
        val toCheck = t.filter(q => !setChessman.beats(q))
        if (toCheck.nonEmpty) returnAllSolutionsForFirstChessman(t, unsetChessPieces, alreadySetChessPieces, result ++ returnSolutionsForSinglePermutation(toCheck, unsetChessPieces.tail, setChessman :: alreadySetChessPieces))
        else
          returnAllSolutionsForFirstChessman(t, unsetChessPieces, alreadySetChessPieces, result)
      }
      else
        returnAllSolutionsForFirstChessman(t, unsetChessPieces, alreadySetChessPieces, result)
  }

  def returnSolutionsForSinglePermutation(freeFields: List[Field], unsetChessPieces: List[UnsetChessman], alreadySetChessPieces: List[Chessman]): List[List[Chessman]] = {
    unsetChessPieces match {
      case Nil => Nil
      case currentPiece :: Nil =>
        freeFields.filterNot(p => p.beatsChessPieces(currentPiece.checkingFunction, alreadySetChessPieces)).
          map(p => currentPiece.setOnField(p) :: alreadySetChessPieces)
      case (_) =>
        returnAllSolutionsForFirstChessman(freeFields, unsetChessPieces, alreadySetChessPieces, Nil)
    }
  }

  def numberOfSinglePermutationSolution1(freeFields: List[Field], unsetChessPieces: List[UnsetChessman], alreadySetChessPieces: List[Chessman]): Int = {
    (freeFields, unsetChessPieces) match {
      case (_, Nil) => 0
      case (h :: t, currentPiece :: Nil) => freeFields.filterNot(p => p.beatsChessPieces(currentPiece.checkingFunction, alreadySetChessPieces)).size
      case (Nil, _) => 0
      case (_, _) =>
        val setChessman = unsetChessPieces.head.setOnField(freeFields.head)
        if (!setChessman.beatsChessPieces(alreadySetChessPieces)) {
          val toCheck = freeFields.tail.filter(q => !setChessman.beats(q))
          if (toCheck.nonEmpty) numberOfSinglePermutationSolution1(freeFields.tail, unsetChessPieces, alreadySetChessPieces) +
            numberOfSinglePermutationSolution1(toCheck, unsetChessPieces.tail, setChessman :: alreadySetChessPieces)
          else
            numberOfSinglePermutationSolution1(freeFields.tail, unsetChessPieces, alreadySetChessPieces)
        }
        else
          numberOfSinglePermutationSolution1(freeFields.tail, unsetChessPieces, alreadySetChessPieces)
    }
  }

  @tailrec
  def singlePermutationNumberOfSolutionsTailRec(listOfCases: List[(List[Field], List[UnsetChessman], List[Chessman])], numberOfAlreadyFoundSolutions: Int): Int = listOfCases match {
    case Nil =>
      numberOfAlreadyFoundSolutions
    case head :: tail =>
      val freeFields = head._1
      val chessPiecesToSet = head._2
      val alreadySetChessPieces = head._3
      (freeFields, chessPiecesToSet) match {
        case (_, Nil) =>
          singlePermutationNumberOfSolutionsTailRec(tail, numberOfAlreadyFoundSolutions)
        case (Nil, _) =>
          singlePermutationNumberOfSolutionsTailRec(tail, numberOfAlreadyFoundSolutions)
        case (h :: t, currentPiece :: Nil) =>
          val tmpNumberOfSolutions = freeFields.count(p => !p.beatsChessPieces(currentPiece.checkingFunction, alreadySetChessPieces))
          singlePermutationNumberOfSolutionsTailRec(tail, numberOfAlreadyFoundSolutions+tmpNumberOfSolutions)
        case (_, _) =>
          val chessManToSet = chessPiecesToSet.head
          val fieldWhenChessmanCanBeSet = freeFields.filterNot(p => p.beatsChessPieces(chessManToSet.checkingFunction, alreadySetChessPieces))
          val newCasesToCheck = fieldWhenChessmanCanBeSet.
            map(p => (freeFields.filter(q => q.isAfter(p) && !chessManToSet.setOnField(p).beats(q)),chessPiecesToSet.tail, chessManToSet.setOnField(p) :: alreadySetChessPieces))
          singlePermutationNumberOfSolutionsTailRec(newCasesToCheck ++ tail, numberOfAlreadyFoundSolutions)
      }
  }

  @tailrec
  def singlePermutationSolutionsTailRec(listOfCases: List[(List[Field], List[UnsetChessman], List[Chessman])], alreadyFoundSolutions: List[List[Chessman]]): List[List[Chessman]] = listOfCases match {
    case Nil =>
      alreadyFoundSolutions
    case head :: tail =>
      val freeFields = head._1
      val chessPiecesToSet = head._2
      val alreadySetChessPieces = head._3
      (freeFields, chessPiecesToSet) match {
        case (_, Nil) =>
          singlePermutationSolutionsTailRec(tail, alreadyFoundSolutions)
        case (Nil, _) =>
          singlePermutationSolutionsTailRec(tail, alreadyFoundSolutions)
        case (h :: t, currentPiece :: Nil) =>
          val newSolutions = freeFields.filter(p => !p.beatsChessPieces(currentPiece.checkingFunction, alreadySetChessPieces)).
            map(p => chessPiecesToSet.head.setOnField(p) :: alreadySetChessPieces)
          singlePermutationSolutionsTailRec(tail, alreadyFoundSolutions ++ newSolutions)
        case (_, _) =>
          val chessManToSet = chessPiecesToSet.head
          val fieldWhenChessmanCanBeSet = freeFields.filterNot(p => p.beatsChessPieces(chessManToSet.checkingFunction, alreadySetChessPieces))
          val newCasesToCheck = fieldWhenChessmanCanBeSet.
            map(p => (freeFields.filter(q => q.isAfter(p) && !chessManToSet.setOnField(p).beats(q)),chessPiecesToSet.tail, chessManToSet.setOnField(p) :: alreadySetChessPieces))
          singlePermutationSolutionsTailRec(newCasesToCheck ++ tail, alreadyFoundSolutions)
      }
  }


}
