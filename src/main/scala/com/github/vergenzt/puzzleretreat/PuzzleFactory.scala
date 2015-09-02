package com.github.vergenzt.puzzleretreat

import scala.io.Source
import scala.language.dynamics

import com.github.vergenzt.util._

object PuzzleFactory {
  val COMMENT_SYMBOL = "%"
  val EOL_WHITESPACE = "\\s+$"

  val SQUARE_MAPPING: Map[Char, Square] = Map(
    '-' -> Empty,
    '1' -> IceSource(1),
    '2' -> IceSource(2),
    '3' -> IceSource(3),
    '4' -> IceSource(4),
    '5' -> IceSource(5),
    '6' -> IceSource(6),
    '7' -> IceSource(7),
    '8' -> IceSource(8),
    '9' -> IceSource(9),
    '#' -> Stump,
    '*' -> Ice
  )

  /**
   * Loads a level file into a sequence of puzzles.
   *
   * Level files are text files with the following format:
   *
   *  * Percent signs ('%') indicate comments, and can begin anywhere in a line.
   *  * Individual levels are sequences of non-blank lines, separated by one or
   *    more blank lines. (Note: Lines with only a comment are considered blank.)
   */
  def fromLevelSource(source: Source): Seq[Puzzle] = {
    // this could probably be done better using regexes (i.e. could avoid
    // splitting lines and then recombining them) but I thought this was
    // pretty clear
    val levelLines = source
      .getLines()
      .map(_.split(COMMENT_SYMBOL, 2)(0))
      .map(_.replaceAll(EOL_WHITESPACE, ""))
      .mkString("\n")
      .split("\n\n+")

    levelLines.map(fromString)
  }

  def fromLevelResource(name: String) =
    fromLevelSource(Source.fromURL(getClass.getResource(name)))

  def fromString(string: String): Puzzle = {
    var (rowMin, rowMax) = (Int.MaxValue, Int.MinValue)
    var (colMin, colMax) = (Int.MaxValue, Int.MinValue)
    val b = Map.newBuilder[Vec, Square]

    for {
      (line, i) <- string.split('\n').zipWithIndex
      (char, j) <- line.zipWithIndex
      square <- SQUARE_MAPPING.get(char)
    } {
      b += (i,j) -> square

      // update {row,col}{Max,Min} only with squares that were recognized
      rowMin = math.min(rowMin, i)
      colMin = math.min(colMin, j)
      rowMax = math.max(rowMax, i)
      colMax = math.max(colMax, j)
    }

    b.result()
      // translate so (rowMin, colMin) is the origin
      .map {
        case ((i, j), square) => ((i - rowMin, j - colMin), square)
      }
  }

  def toString(puzzle: Puzzle): String = {
    import math.{min, max}
    var (rowMin, colMin, rowMax, colMax) = (0, 0, 0, 0)
    puzzle.keys.foreach { case (row, col) =>
      rowMin = min(rowMin, row)
      colMin = min(colMin, col)
      rowMax = max(rowMax, row)
      colMax = max(colMax, col)
    }

    val b = new StringBuilder
    for (row <- rowMin to rowMax) {
      for (col <- colMin to colMax) {
        b += {
          puzzle.get((row,col)) match {
            case Some(square) =>
              PuzzleFactory.SQUARE_MAPPING.find(_._2 == square) match {
                case Some((char, _)) => char
                case None => '?' // no mapping found for square
              }
            case None => ' ' // no square at location
          }
        }
      }
      b += '\n'
    }
    b.result()
  }

  object predef extends Dynamic {
    private val PACKAGE_PREFIX = "/" + getClass.getPackage.getName.replace('.', '/')
    private def predefLevelSet(levelName: String) = fromLevelResource(s"$PACKAGE_PREFIX/$levelName.txt")

    // predefined level sets
    val welcome = predefLevelSet("welcome")
  }
}
