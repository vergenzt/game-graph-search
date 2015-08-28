package com.github.vergenzt.puzzleretreat

import com.github.vergenzt.util._

case class Puzzle(
  minCorner: Vec,
  maxCorner: Vec,
  state: Map[Vec, Square]
) {

  /**
   * Returns true if the puzzle is solved.
   */
  def isSolved: Boolean = {
    !state.values.exists(_.isInstanceOf[DynamicSquare])
  }

  override def toString(): String = {
    val b = new StringBuilder
    val (rowMin, colMin) = minCorner
    val (rowMax, colMax) = maxCorner
    for (row <- rowMin to rowMax) {
      for (col <- colMin to colMax) {
        b += {
          state.get((row,col)) match {
            case Some(square) =>
              Puzzle.SQUARE_MAPPING.find(_._2 == square) match {
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

object Puzzle {
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
    '#' -> Stump[IceSource](),
    '*' -> Ice
  )

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

    val minCorner = (0, 0)
    val maxCorner = (rowMax - rowMin, colMax - colMin)

    Puzzle(minCorner, maxCorner, b.result())
  }
}
