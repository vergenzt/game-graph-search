package com.github.vergenzt

import com.github.vergenzt.util._
import scala.collection.mutable

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
    ???
  }

  /**
   *
   */
  def executeStep(puzzle: Puzzle, pos: Vec, dir: Dir, activeSquare: DynamicSquare)
    : Option[(Puzzle, Option[(Vec, Dir, DynamicSquare)])] = {

    ???
  }

  type MoveState = (DynamicSquare, Dir)
  type MoveFn = PartialFunction[
    (StaticSquare, MoveState),
    (StaticSquare, Option[MoveState])
  ]

  def handleMovement: PartialFunction[
    (StaticSquare,
}
