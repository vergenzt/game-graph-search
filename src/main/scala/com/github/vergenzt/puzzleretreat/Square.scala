package com.github.vergenzt.puzzleretreat

import com.github.vergenzt.util._
import scala.util.Try

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
 */
abstract class DynamicSquare extends Square {
  def execute(puzzle: Puzzle, action: Action): Option[Puzzle] = {
    val (pos, dir) = action
    val square: DynamicSquare = puzzle(pos).asInstanceOf[DynamicSquare]

    move(puzzle.updated(pos, Stump), pos + dir.vec, dir)
  }

  def move(puzzle: Puzzle, pos: Vec, dir: Dir): Option[Puzzle]
}

/* Base static squares. */

case object Empty extends StaticSquare

/* Dynamic squares. */

case class IceSource(n: Int) extends DynamicSquare {
  override def move(puzzle: Puzzle, pos: Vec, dir: Dir): Option[Puzzle] =
    puzzle.get(pos) match {
      case _ if n == 0 => Some(puzzle)
      case Some(Empty) => IceSource(n-1).move(puzzle.updated(pos, Ice), pos + dir.vec, dir)
      case Some(Ice)   => move(puzzle, pos + dir.vec, dir)
      case _ => None
    }
}

case object Fire extends DynamicSquare {
  override def move(puzzle: Puzzle, pos: Vec, dir: Dir): Option[Puzzle] =
    puzzle.get(pos) match {
      case Some(Empty) => Some(puzzle.updated(pos, FireBlock))
      case Some(Ice)   =>
        move(puzzle.updated(pos, Empty), pos + dir.vec, dir)
          .orElse(Some(puzzle.updated(pos, FireBlock)))
      case _ => None
    }
}

case object BlockerSource extends DynamicSquare {
  override def move(puzzle: Puzzle, pos: Vec, dir: Dir): Option[Puzzle] =
    puzzle.get(pos) match {
      case Some(Empty) => Some(puzzle.updated(pos, Blocker))
      case Some(Ice)   => move(puzzle, pos + dir.vec, dir)
      case _ => None
    }
}

/* Leftover static squares. */

case object Ice extends StaticSquare
case object FireBlock extends StaticSquare
case object Blocker extends StaticSquare

/** The leftover square at the original location of a DynamicSquare. */
case object Stump extends StaticSquare
