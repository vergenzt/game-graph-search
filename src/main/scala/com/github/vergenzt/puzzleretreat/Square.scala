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
 */
abstract class DynamicSquare extends Square {
  def transformSelf: PartialFunction[StaticSquare, Option[DynamicSquare]]
  def transform: PartialFunction[StaticSquare, StaticSquare]
}

/* Base static squares. */

case object Empty extends StaticSquare

/* Dynamic squares. */

case class IceSource(n: Int) extends DynamicSquare {
  override def transform = {
    case Ice => Ice
    case Empty => Ice
  }
  override def transformSelf = {
    case Ice => Some(this)
    case Empty => Some(IceSource(n-1)).filter(_.n > 1)
  }
}

/* Leftover static squares. */

case object Ice extends StaticSquare

/** The leftover square at the original location of a DynamicSquare. */
case object Stump extends StaticSquare
