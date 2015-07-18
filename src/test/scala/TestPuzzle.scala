import org.scalatest.FlatSpec

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
 * Created by bartek on 7/16/15.
 */
class TestPuzzle extends FlatSpec {

  "Two kings, one rook. Chessboard 3x3" should "4" in {
    val king = Chessman('k')
    val rook = Chessman('r')
    val chessmen = List(king, king, rook)
    val chessBoard = ChessBoard.createChessBoard(3)

    val listOfFutures = chessmen.permutations.collect { case p =>
      Future {
        Algorithm.numberOfSinglePermutationSolution(chessBoard, p.flatten, List())
      }
    }
    val futureOfList = Future.sequence(listOfFutures)
    val sum = Await.result(futureOfList, 30000.milliseconds).sum

    assert(sum == 4)
  }

  "Two rooks, four knights. Chessboard 4x4" should "8" in {
    val knight = Chessman('s')
    val rook = Chessman('r')
    val chessmen = List(rook, rook, knight, knight, knight, knight)
    val chessBoard = ChessBoard.createChessBoard(4)

    val listOfFutures = chessmen.permutations.collect { case p =>
      Future {
        Algorithm.numberOfSinglePermutationSolution(chessBoard, p.flatten, List())
      }
    }
    val futureOfList = Future.sequence(listOfFutures)
    val sum = Await.result(futureOfList, 30000.milliseconds).sum

    assert(sum == 8)
  }

  "Eight rooks. Chessboard 8x8" should "40320" in {
    val rook = Chessman('r')
    val chessmen = List(rook, rook, rook, rook, rook, rook, rook, rook)
    val chessBoard = ChessBoard.createChessBoard(8)

    val listOfFutures = chessmen.permutations.collect { case p =>
      Future {
        Algorithm.numberOfSinglePermutationSolution(chessBoard, p.flatten, List())
      }
    }
    val futureOfList = Future.sequence(listOfFutures)
    val sum = Await.result(futureOfList, 30000.milliseconds).sum

    assert(sum == 40320)
  }

  "Eight queens. Chessboard 8x8" should "92" in {
    val queen = Chessman('q')
    val chessmen = List(queen, queen, queen, queen, queen, queen, queen, queen)
    val chessBoard = ChessBoard.createChessBoard(8)

    val listOfFutures = chessmen.permutations.collect { case p =>
      Future {
        Algorithm.numberOfSinglePermutationSolution(chessBoard, p.flatten, List())
      }
    }
    val futureOfList = Future.sequence(listOfFutures)
    val sum = Await.result(futureOfList, 30000.milliseconds).sum

    assert(sum == 92)
  }

  "Two kings, two queens, two bishops, one knight. Chessboard 7x7" should "3063828" in {
    val king = Chessman('k')
    val queen = Chessman('q')
    val bishop = Chessman('b')
    val knight = Chessman('s')
    val chessmen = List(king, king, queen, queen, bishop, bishop, knight)
    val chessBoard = ChessBoard.createChessBoard(7)

    val listOfFutures = chessmen.permutations.collect { case p =>
      Future {
        Algorithm.numberOfSinglePermutationSolution(chessBoard, p.flatten, List())
      }
    }
    val futureOfList = Future.sequence(listOfFutures)
    val sum = Await.result(futureOfList, 30000.milliseconds).sum

    assert(sum == 3063828)
  }

  "TailRec Solution:: Two kings, one rook. Chessboard 3x3" should "4" in {
    val king = Chessman('k')
    val rook = Chessman('r')
    val chessmen = List(king, king, rook)
    val chessBoard = ChessBoard.createChessBoard(3)

    val listOfFutures = chessmen.permutations.collect { case p =>
      Future {
        Algorithm.singlePermutationSolutionTailRec(List((chessBoard, p.flatten, List())), List()).size
      }
    }
    val futureOfList = Future.sequence(listOfFutures)
    val sum = Await.result(futureOfList, 30000.milliseconds).sum


    assert(sum == 4)
  }

  "TailRec Solution. Two rooks, four knights. Chessboard 4x4" should "8" in {
    val knight = Chessman('s')
    val rook = Chessman('r')
    val chessmen = List(rook, rook, knight, knight, knight, knight)
    val chessBoard = ChessBoard.createChessBoard(4)

    val listOfFutures = chessmen.permutations.collect { case p =>
      Future {
        Algorithm.singlePermutationNumberOfSolutionTailRec(List((chessBoard, p.flatten, List())), 0)
      }
    }
    val futureOfList = Future.sequence(listOfFutures)
    val sum = Await.result(futureOfList, 30000.milliseconds).sum

    assert(sum == 8)
  }

    "TailRec Solution. Eight rooks. Chessboard 8x8" should "40320" in {
      val rook = Chessman('r')
      val chessmen = List(rook, rook, rook, rook, rook, rook, rook, rook)
      val chessBoard = ChessBoard.createChessBoard(8)

      val listOfFutures = chessmen.permutations.collect { case p =>
        Future {
          Algorithm.singlePermutationNumberOfSolutionTailRec(List((chessBoard, p.flatten, List())), 0)
        }
      }
      val futureOfList = Future.sequence(listOfFutures)
      val sum = Await.result(futureOfList, 30000.milliseconds).sum

      assert(sum == 40320)
    }

    "TailRec Solution. Eight queens. Chessboard 8x8" should "92" in {
      val queen = Chessman('q')
      val chessmen = List(queen, queen, queen, queen, queen, queen, queen, queen)
      val chessBoard = ChessBoard.createChessBoard(8)

      val listOfFutures = chessmen.permutations.collect { case p =>
        Future {
          Algorithm.singlePermutationNumberOfSolutionTailRec(List((chessBoard, p.flatten, List())), 0)
        }
      }
      val futureOfList = Future.sequence(listOfFutures)
      val sum = Await.result(futureOfList, 30000.milliseconds).sum

      assert(sum == 92)
    }

    "TailRec Solution. Two kings, two queens, two bishops, one knight. Chessboard 7x7" should "3063828" in {
      val king = Chessman('k')
      val queen = Chessman('q')
      val bishop = Chessman('b')
      val knight = Chessman('s')
      val chessmen = List(king, king, queen, queen, bishop, bishop, knight)
      val chessBoard = ChessBoard.createChessBoard(7)

      val listOfFutures = chessmen.permutations.collect { case p =>
        Future {
          Algorithm.singlePermutationNumberOfSolutionTailRec(List((chessBoard, p.flatten, List())), 0)
        }
      }
      val futureOfList = Future.sequence(listOfFutures)
      val sum = Await.result(futureOfList, 5000000.milliseconds).sum

      assert(sum == 3063828)
    }

}
