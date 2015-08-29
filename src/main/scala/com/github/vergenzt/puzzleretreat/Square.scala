package com.github.vergenzt.puzzleretreat

import com.github.vergenzt.util._

/**
 * Base trait representing all squares in the game.
 */
sealed trait Square

/**
 * A square that cannot be moved directly by the player.
 */
sealed trait StaticSquare extends Square

/**
 * A square that can be moved in a direction by the player.
 *
 * The sequence of events that happens to determine the behavior of this type
 * of square is as follows:
 *
 *  1. Move is issued in a direction `dir`.
 *  2. Recursively evolve the DynamicSquare along the interaction function
 *     until either (a) the interaction function is not defined for the next
 *     input, in which case the move fails; or (b) the return value of the
 *     function does not include the optional (DynamicSquare, Dir) tuple with
 *     which to continue, in which case the move ends successfully.
 *  3. If the move is successful, the original location of this DynamicSquare
 *     is replaced in the grid with Stump.
 */
abstract class DynamicSquare extends Square {

  /** The state of a moving DynamicSquare */
  type MoveState = (DynamicSquare, Dir)
  type MoveFn = PartialFunction[
    (StaticSquare, Dir),
    (StaticSquare, Option[(DynamicSquare, Dir)])
  ]

  /**
   * Partial function defining how this square interacts with other squares.
   *
   * Input is the next Square the DynamicSquare will interact with and
   * the direction of movement. Output is the Square left over after the
   * interaction, and an optional DynamicSquare that keeps moving in the
   * specified direction.
   */
  def move: MoveFn
}

/* Base static squares. */

case object Empty extends StaticSquare

/* Dynamic squares. */

case class IceSource(n: Int) extends DynamicSquare {
  override val moveFn: MoveFn = {
    case (Ice, dir) => (Ice, Some((this, dir)))
    case (Empty, dir) => (
      Ice,
      if (n <= 1)
        None
      else
        Some(IceSource(n-1), dir)
    )
  }
}

/* Leftover static squares. */

case object Ice extends StaticSquare

/** The leftover square at the original location of a DynamicSquare. */
case object Stump extends StaticSquare
