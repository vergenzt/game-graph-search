package com.github.vergenzt.puzzleretreat

import scala.io.Source
import com.github.vergenzt.searcher.Searcher

object Runner {

  def main(args: Array[String]): Unit = {
    val level = PuzzleFactory.fromString("""
  --
1----1
1----2
2----1
 2BB2
    """)

    Searcher.bfs(level, neighbors, solved) match {
      case Some((moves, solution)) =>
        println(s"""
          |Found solution!
          |
          |Moves:
          |${moves.map { case (pos,dir) => s" Move ${level(pos)} at $pos in direction $dir" }.mkString("\n")}
          |
          |Final state:
          |${PuzzleFactory.toString(solution)}
        """.stripMargin)

      case None => println("No solution found! :(")
    }

  }

}
