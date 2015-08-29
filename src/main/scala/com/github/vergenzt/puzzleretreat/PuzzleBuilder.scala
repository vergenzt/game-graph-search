package com.github.vergenzt.puzzleretreat

import scala.io.Source
import com.github.vergenzt.util._
import java.util.Comparator

object PuzzleBuilder {
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
   * Loads a level file into a sequence of named puzzles.
   *
   * Level files are text files with the following format:
   *
   *  * Percent signs ('%') indicate comments, and can begin anywhere in a line.
   *  * Individual levels are sequences of non-blank lines, separated by one or
   *    more blank lines. (Note: Lines with only a comment are considered blank.)
   */
  def loadLevels(source: Source): Seq[Puzzle] = {
    val levelLines = source
      .getLines()
      .map(_.split(COMMENT_SYMBOL, 1)(0))
      .map(_.replaceAll(EOL_WHITESPACE, ""))
      .mkString("\n")
      .replaceAll("\n\n+", "\n\n")
      .split("\n\n")

    levelLines.map(fromString)
  }

  def loadLevelsFromResource(name: String) =
    loadLevels(Source.fromURL(getClass.getResource(name)))

  def fromString(string: String): Puzzle = {
    var (rowMin, rowMax, colMin, colMax) = (0, 0, 0, 0)
    val b = Map.newBuilder[Vec, Square]

    for {
      (line, i) <- string.split('\n').iterator.zipWithIndex
      (char, j) <- line.zipWithIndex
    } {
      SQUARE_MAPPING.get(char).foreach(b += (i,j) -> _)
      rowMin = math.min(rowMin, i)
      colMin = math.min(colMin, j)
      rowMax = math.max(rowMax, i)
      colMax = math.max(colMax, j)
    }

    // translate so (row0, col0) is the origin
    b.mapResult(state => state.map {
      case ((i, j), square) => ((i - rowMin, j - colMin), square)
    })

    b.result()
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
              PuzzleBuilder.SQUARE_MAPPING.find(_._2 == square) match {
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
}
