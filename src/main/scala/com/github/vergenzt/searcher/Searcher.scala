package com.github.vergenzt.searcher

import scala.collection.mutable

object Searcher {
  def bfs[N](start: N, neighbors: N => Seq[N], isGoal: N => Boolean): Option[N] = {
    val frontier = mutable.Queue[N](start)
    val visited = mutable.Set[N]()
    while (!frontier.isEmpty) {
      val x = frontier.dequeue()
      if (isGoal(x))
        Some(x)
      else {
        visited += x
        frontier ++= neighbors(x).filterNot(visited)
      }
    }
    None
  }
}
