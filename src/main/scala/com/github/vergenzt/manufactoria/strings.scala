package com.github.vergenzt.manufactoria

import com.github.vergenzt.util.Cardinal._

object strings {

  val componentChars: Seq[(Component, String)] = Seq(
    Empty -> " ",
    Conveyor(N) -> "^",
    Conveyor(S) -> "v",
    Conveyor(E) -> ">",
    Conveyor(W) -> "<",
    ???
  )

}
