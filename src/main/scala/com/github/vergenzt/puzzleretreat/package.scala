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
      (pos, _: DynamicSquare) <- puzzle.iterator
      dir <- Cardinals
      action = (pos, dir)
      neighbor <- execute(puzzle, action)
    } yield {
      (action, neighbor)
    }
  }

  /**
   * Simulates the given move on the given puzzle state, returning the final
   * state if the move is successful.
   *
   * @param puzzle the puzzle on which to execute the action
   * @param action the (position, direction) pair to move
   * @return the resulting state if the move is valid, None otherwise
   */
  def execute(puzzle: Puzzle, action: Action): Option[Puzzle] = {
    Try {
      var curPuzzle = mutable.Map(puzzle.toSeq: _*)
      var (curPos, curDir) = action
      var curDynamic: Option[DynamicSquare] = Some(puzzle(curPos).asInstanceOf[DynamicSquare])

      curPuzzle(curPos) = Stump
      curPos += curDir.vec

      while (curDynamic.isDefined) {
        val curStatic = curPuzzle(curPos).asInstanceOf[StaticSquare]

        curPuzzle(curPos) = curDynamic.get.transform(curStatic)
        curDynamic = curDynamic.get.transformSelf(curStatic)

        curPos += curDir.vec
      }
      curPuzzle.toMap
    }.toOption
  }
}
