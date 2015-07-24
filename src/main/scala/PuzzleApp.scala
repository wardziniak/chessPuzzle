import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


/**
 * Created by bartek on 7/13/15.
 * Main class
 */
object PuzzleApp {

  def main(args: Array[String]): Unit = {
    val queen = Chessman('q')
    val bishop = Chessman('b')
    val king = Chessman('k')
    val knight = Chessman('s')

    val s = System.currentTimeMillis

    val chessmen = List(king, king, queen, queen, bishop, bishop, knight)
    val chessBoard = ChessBoard.createChessBoard(7)

    val listOfFutures = chessmen.permutations.collect { case p =>
      Future {
        Algorithm.singlePermutationNumberOfSolutionsTailRec(List(SingleCase(chessBoard, p.flatten, List())), 0)
      }
    }
    val futureOfList = Future.sequence(listOfFutures)
    val sum = Await.result(futureOfList, 30000.milliseconds).sum

    println(s"Number of solutions: $sum")
    println(s"Execution time: ${System.currentTimeMillis - s}")


  }


}
