package com.github.vergenzt.manufactoria

import types.Code
import types.Decision

abstract class Problem {
  def description: String

  /** The starter machine with size constraints and fixed components. */
  def template: Machine

  /** A list of manually-specified test cases for the problem. */
  def testCases: Seq[Code]

  /** A function to compute the actual result for a test case. */
  def acceptFn(code: Code): Decision

  /**
   * Check whether the given solution solves the problem for all provided test cases.
   * @return Some(Reject) if any test cases fail, Some(Accept) if all pass, or None otherwise
   */
  def checkMachine(grid: Machine, maxIterations: Int = 10000): Decision =
    testCases.forall(code =>
      grid.process(code, maxIterations) == Some(acceptFn(code))
    )
}
