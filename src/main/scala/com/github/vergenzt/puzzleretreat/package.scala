package com.github.vergenzt

import scala.collection.mutable
import scala.util.Try

import com.github.vergenzt.util._

package object puzzleretreat {

  type Puzzle = Map[Vec, Square]
  type Action = (Vec, Cardinal)

  /**
   * Returns true if the puzzle is solved.
   */
  def solved(puzzle: Puzzle): Boolean = {
    !puzzle.values.exists(_.isInstanceOf[DynamicSquare])
  }

  /**
   * Returns the set of  actions to take.
   */
  def neighbors(puzzle: Puzzle): Iterator[(Action, Puzzle)] = {
    for {
      (pos, square: DynamicSquare) <- puzzle.iterator
      dir <- Cardinals
      action = (pos, dir)
      neighbor <- square.execute(puzzle, action)
    } yield {
      (action, neighbor)
    }
  }
}
