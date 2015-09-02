package com.github.vergenzt.searcher

import scala.collection.mutable

object Searcher {
  def bfs[N, E](start: N, neighbors: N => Iterator[(E, N)], isGoal: N => Boolean): Option[(Seq[E], N)] = {
    val frontier = mutable.Queue[N](start)
    val parent = mutable.Map[N, (N, E)]()
    while (!frontier.isEmpty) {
      val x = frontier.dequeue()
      if (isGoal(x)) {
        val path = Seq.newBuilder[E]
        var curY = x
        while (parent.contains(curY)) {
          val (curX, curXY) = parent(curY)
          path += curXY
          curY = curX
        }
        return Some((path.result().reverse, x))
      }
      else {
        neighbors(x)
          .foreach { case (xy, y) =>
            frontier += y
            parent(y) = (x, xy)
          }
      }
    }
    return None
  }
}
