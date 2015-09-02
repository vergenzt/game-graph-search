package com.github.vergenzt.puzzleretreat

import scala.io.Source
import com.github.vergenzt.searcher.Searcher

object Runner {

  def main(args: Array[String]): Unit = {
    implicit def show(puzzle: Puzzle): Unit = println(PuzzleFactory.toString(puzzle))

    val level = PuzzleFactory.fromString("""
        1
      1--
       --1
       1
    """)

    println(solved(level))

    Searcher.bfs(level, neighbors, solved) match {
      case Some((moves, solution)) =>
        println(s"""
          |Found solution!
          |
          |Moves:
          |${moves.map { case (pos,dir) => s" Move $pos $dir" }.mkString("\n")}
          |
          |Final state:
          |${PuzzleFactory.toString(solution)}
        """.stripMargin)

      case None => println("No solution found! :(")
    }

  }

}
