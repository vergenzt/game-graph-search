package com.github.vergenzt.manufactoria

import types._
import com.github.vergenzt.util._

/** A single component at a single location in a gridimport com.github.vergenzt.manufactoria.types
. */
sealed trait Component {
  def step(state: State): State

  override def toString(): String = {
    import Cardinal._, Dot._
    this match {
      case Empty         => " "
      case Start(`N`)    => "▲"
      case Start(`S`)    => "▼"
      case Start(`E`)    => "▶"
      case Start(`W`)    => "◀"
      case End()         => "■"
      case Conveyor(`N`) => "↑"
      case Conveyor(`S`) => "↓"
      case Conveyor(`E`) => "→"
      case Conveyor(`W`) => "←"
      case _             => "?"
    }
  }
}

// component implementations

case object Empty extends Component {
  override def step(state: State): State = state.copy(decision = Some(Reject))
}

case class Start(dir: Cardinal) extends Component {
  override def step(state: State): State = if (state.vel.isZero) state.moved(dir)
    else state.copy(decision = Some(Reject))
}

case class End() extends Component {
  override def step(state: State): State = state.copy(decision = Some(Accept))
}

case class Conveyor(dir: Cardinal) extends Component {
  override def step(state: State): State = state.moved(dir)
}

case class Branch(dir: Vec, lcolor: Dot, rcolor: Dot) extends Component {
  override def step(state: State): State = state.code match {
    case `lcolor` :: rest => state.copy(
      code = rest,
      pos = state.pos + dir.left,
      vel = dir.left
    )
    case `rcolor` :: rest => state.copy(
      code = rest,
      pos = state.pos + dir.right,
      vel = dir.right
    )
    case _ => state.copy(
      pos = state.pos + dir,
      vel = dir
    )
  }
}

case class Writer(dir: Vec, color: Dot) extends Component {
  override def step(state: State): State = state.copy(
    code = state.code :+ color,
    pos = state.pos + dir,
    vel = dir
  )
}
