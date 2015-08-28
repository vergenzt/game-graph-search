package com.github.vergenzt.manufactoria.problems

import com.github.vergenzt.manufactoria._
import com.github.vergenzt.util._

import types._
import Cardinal._
import Dot._

object AcceptAll extends Problem {
  override val description: String = "Accept all robots"

  override val template: Machine = new Machine {
    override val size: Vec = (5, 5)
    override val components = Map(
      (1,3) -> Start(S),
      (5,3) -> End()
    )
  }

  override val testCases: Seq[Code] = Seq(
    Seq(B, R, B, B, R),
    Seq(R, B, B)
  )

  override def acceptFn(code: Code): Boolean = true
}

object StartsWithBlue extends Problem {
  override val description: String = "Accept robots that start with a blue"

  override val template: Machine = new Machine {
    override val size: Vec = (5, 5)
    override val components = Map(
      (1,3) -> Start(S),
      (5,3) -> End()
    )
  }

  override val testCases: Seq[Code] = Seq(
    Seq(B, R, B, B, R),
    Seq(R, B, B)
  )

  override def acceptFn(code: Code): Decision = code.head == B
}

object ThreeOrMoreBlues extends Problem {
  override val description: String = "Accept robots with three or more blues"

  override val template: Machine = new Machine {
    override val size: Vec = (7, 7)
    override val components = Map(
      (1,4) -> Start(S),
      (7,4) -> End()
    )
  }

  override val testCases: Seq[Code] = Seq(
    Seq(B, R, B, B, R),
    Seq(R, B, B)
  )

  override def acceptFn(code: Code): Decision = code.count(_ == B) >= 3
}
