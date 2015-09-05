package org.orbitfold.iafs

import scala.collection.mutable.LinkedList

class Optimizer(problemc: Problem, initial: Vector[Interval]) {
  val problem: Problem = problemc
  var intervals: LinkedList[Vector[Interval]] = LinkedList(initial)
  var best_x: Vector[Interval] = 
    for (interval <- initial) yield new Interval(interval.midpoint)
  var best_y: Interval = problem(best_x)

  def bisect(xs: Vector[Interval]) : LinkedList[Vector[Interval]] = {
    def narrowest(i: Integer, narrowest_i: Integer): Integer = {
      if (i >= xs.length) {
        narrowest_i
      } else if (xs(i).width <= xs(narrowest_i).width) {
        narrowest(i + 1, i)
      } else {
        narrowest(i + 1, narrowest_i)
      }
    }
    val narrowest_i = narrowest(0, 0)
    val intervals1 = xs.updated(narrowest_i, 
      new Interval(xs(narrowest_i).a, xs(narrowest_i).midpoint))
    val intervals2 = xs.updated(narrowest_i,
      new Interval(xs(narrowest_i).midpoint, xs(narrowest_i).b))
    LinkedList(intervals1, intervals2)
  }

  def advance = {
    val result = problem(intervals.head)
    if (result.a > best_y.b) {
      intervals = intervals.tail
    } else {
      intervals.append(bisect(intervals.head))
      val midpoint = for (interval <- intervals.head) yield new Interval(interval.midpoint)
      val y = problem(midpoint)
      if (y < best_y) {
        best_x = midpoint
        best_y = y
      }
    }
  }

  def optimize(tolerance: Double) = {
    while ((for (interval <- intervals.head) yield interval.width).min > tolerance) {
      advance
    }
  }
}
