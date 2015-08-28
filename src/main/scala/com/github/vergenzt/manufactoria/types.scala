package com.github.vergenzt.manufactoria

import com.github.vergenzt.util._

object types {

  sealed trait Dot
  object Dot {
    case object B extends Dot
    case object R extends Dot
    case object G extends Dot
    case object Y extends Dot
  }

  type Code = Seq[Dot]

  type Decision = Boolean
  val Accept: Decision = true
  val Reject: Decision = false

  case class State(code: Code, pos: Vec, vel: Vec, decision: Option[Decision]) {
    def moved(direction: Cardinal): State = this.copy(pos = pos + direction.vec, vel = direction.vec)
  }
}
