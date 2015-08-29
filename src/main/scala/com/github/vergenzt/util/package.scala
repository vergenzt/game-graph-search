package com.github.vergenzt

package object util {
  /**
   * A (row, column) representation of a 2D vector.
   *
   * Top-left corner is (0, 0).
   */
  type Vec = (Int, Int)

  implicit class VecOps(rc: Vec) {
    @inline private def r: Int = rc._1
    @inline private def c: Int = rc._2

    def +(uv: Vec): Vec = (r + uv._1, c + uv._2)
    def *(i: Int): Vec = (r * i, c * i)
    def dot(uv: Vec): Int = (r * uv._1) + (c * uv._2)
    def perpendicularTo(that: Vec): Boolean = (this dot that) == 0
    def left: Vec = (-c, r)
    def right: Vec = (c, -r)
    def isZero: Boolean = (r == 0) && (c == 0)

    def boundedBy(size: (Int,Int)): Boolean =
      (0 < r) && (r <= size._1) &&
      (0 < c) && (c <= size._2)
  }

  /** Base type for cardinal direction vectors. */
  sealed trait Cardinal {
    def vec: Vec
  }
  type Dir = Cardinal // alias

  object Cardinal {
    case object N extends Cardinal { override val vec = (-1, 0) }
    case object S extends Cardinal { override val vec = ( 1, 0) }
    case object E extends Cardinal { override val vec = ( 0, 1) }
    case object W extends Cardinal { override val vec = ( 0,-1) }
  }
  val Cardinals = {
    import Cardinal._
    Seq(N, S, E, W)
  }
}
