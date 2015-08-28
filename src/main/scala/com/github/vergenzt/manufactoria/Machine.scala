package com.github.vergenzt.manufactoria

import scala.collection.mutable

import types._
import com.github.vergenzt.util.Vec

abstract class Machine {
  /** The map of components keyed by (row, column). */
  def components: Map[Vec, Component]

  /**
   * Get the component to use for a location on the grid.
   * @return the component if it exists, or Empty otherwise
   */
  def apply(pos: Vec): Component = components.getOrElse(pos, Empty)

  /**
   * Get a specified component from the grid if it exists.
   * @return Some(component) if it exists, None otherwise
   */
  def get(pos: Vec): Option[Component] = components.get(pos)

  /** The (rows, columns) size of the grid. */
  def size: (Int, Int)

  /** The location of the Start component. */
  def startPos: Vec = components
    .collectFirst { case (startPos, start: Start) => startPos }
    .get

  /** The Start component itself. */
  def startComponent: Start = components(startPos).asInstanceOf[Start]

  /** The location of the End component. */
  def endPos: Vec = components
    .collectFirst { case (endPos, end: End) => endPos }
    .get

  /** The End component itself. */
  def endComponent: End = components(endPos).asInstanceOf[End]

  /**
   * Process a given code from the start state and make a decision.
   * @return the decision, if one is made within the iteration limit, or None otherwise
   */
  def process(code: Code, maxIterations: Int = 10000): Option[Decision] = {
    val start = State(code, startPos, (0,0), None)
    Iterator
      .iterate(start)(state => {
        this(state.pos).step(state)
      })
      .map(state => {
        println(s"State: $state")
        state
      })
      .take(maxIterations)
      .collectFirst {
        case State(_, _, _, Some(decision)) => decision
      }
  }

  /*require(components.keys.forall(_.boundedBy(size)),
      "all components must be within size bound")
  require(components.values.count(_.isInstanceOf[Start]) == 1,
      "there must be exactly one Start component")
  require(components.values.count(_.isInstanceOf[End]) == 1,
      "there must be exactly one End component")*/

  override def toString(): String = {
    val b = StringBuilder.newBuilder
    for (row <- 1 to size._1) {
      for (col <- 1 to size._2) {
        b ++= this((row,col)).toString()
      }
      b += '\n'
    }
    b.result()
  }

}

class MutableMachine(base: Machine) extends Machine {
  /* Use the size of the base. */
  override def size: Vec = base.size

  /* Use base components where defined, otherwise this solutions components. */
  override def components: Map[Vec,Component] =
    base.components.withDefault(modifiableComponents)

  /** This solution's modifiable map of components. */
  protected val modifiableComponents = mutable.Map[Vec,Component]()

}
