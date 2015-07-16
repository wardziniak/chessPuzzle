import org.scalatest.FlatSpec

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
 * Created by bartek on 7/16/15.
 */
class TestPuzzle extends FlatSpec {

  "Dwa króle i wieza. Szachownica 3x3" should "4" in {
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

  "Dwie wieze i 4 skoczki. Szachownica 4x4" should "8" in {
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

  "Osiem wiez. Szachownica 8x8" should "40320" in {
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

  "Osiem hetmanow. Szachownica 8x8" should "92" in {
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

  "Dwa króle, dwa hetmany, dwa gońce, jeden skoczek. Szachownica 7x7" should "3063828" in {
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

}
