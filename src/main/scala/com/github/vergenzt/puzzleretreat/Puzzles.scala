package com.github.vergenzt.puzzleretreat

object Puzzles {

  object Welcome {
    lazy val P1 = Puzzle.fromString("""\
      |  1
      |1--
      | --1
      | 1
    """.stripMargin)
  }

  def main(args: Array[String]) = {
    println(Welcome.P1.toString())
  }
}
