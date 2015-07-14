import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


/**
 * Created by bartek on 7/13/15.
 */
object PuzzleApp {

  def main(args: Array[String]): Unit = {
    val queen = Chessman('q')
    val bishop = Chessman('b')
    val king = Chessman('k')
    val rook = Chessman('r')
    val knigh = Chessman('s')
    //val chessmen = List(queen, queen, queen, queen, queen, queen, queen,queen)
    //val chessmens = List(king, king, rook)
    //val chessmens = List(rook, rook, knigh, knigh, knigh, knigh)
    //val chessmens = List(rook, rook, rook)
    val chessmen = List(king, king, queen, queen, bishop, bishop, knigh)

    val chessBoard = new ChessBoard(7)
    //println(chessBoard.free)

    val s = System.currentTimeMillis
    //chessmens.permutations.collect{p => Algorithm.singlePermutationSolution(chessBoard.free, p.flatten, List())}
//    var sum = 0
//    chessmen.permutations.foreach(
//      p => {
//        //println(p)
//        sum += Algorithm.singlePermutationSolution(chessBoard.free, p.flatten, List())
//        //println(sum)
//      })

//    val sumOfResult = chessmen.permutations.collect{ case p =>
//      Algorithm.singlePermutationSolution(chessBoard.free, p.flatten, List())
//      }.sum

    val listOfFutures = chessmen.permutations.collect{ case p =>
        Future {Algorithm.singlePermutationSolution(chessBoard.free, p.flatten, List())}
    }
    val futureOfList = Future.sequence(listOfFutures)
    val sum1 = Await.result(futureOfList, 30000.milliseconds).sum

    //println(s"Number of solutions: ${sumOfResult}")
    println(s"Number of solutions: $sum1")
    println(s"Execution time: ${System.currentTimeMillis - s}")


  }


}
