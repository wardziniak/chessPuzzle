/**
 * Created by bartek on 7/13/15.
 */
object PuzzleApp {

  def main(args: Array[String]): Unit = {
    val chessBoard = new ChessBoard(4)
    val queen = Chessman('q')
    val bishop = Chessman('b')
    val king = Chessman('k')
    val chessmens = List(queen, queen, queen, queen)

    val numberOfSolution = chessmens.permutations.foreach(p => println(Algorithm.singlePermutationSolution(chessBoard.free, p.flatten)))
    //println(numberOfSolution)


  }


}
