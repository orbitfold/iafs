package org.orbitfold.iafs

import scala.collection.mutable.LinkedList

class Optimizer(problemc: Problem, initial: Vector[Interval]) {
  val problem: Problem = problemc
  var intervals: LinkedList[Vector[Interval]] = LinkedList(initial)
  var best_x: Vector[Interval] = 
    for (interval <- initial) yield new Interval(interval.midpoint)
  var best_y: Interval = problem(best_x)
  var best_x_interval: Vector[Interval] = initial
  var best_y_interval: Interval = problem(best_x_interval)

  def bisect(xs: Vector[Interval]) : LinkedList[Vector[Interval]] = {
    val widest_i = (for (x <- xs) yield x.width).zipWithIndex.max._2
    val intervals1 = xs.updated(widest_i, 
      new Interval(xs(widest_i).a, xs(widest_i).midpoint))
    val intervals2 = xs.updated(widest_i,
      new Interval(xs(widest_i).midpoint, xs(widest_i).b))
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
        best_x_interval = intervals.head
        best_y_interval = result
      }
      intervals = intervals.tail
    }
    result.width
  }

  def optimize(tolerance: Double) = {
    while (best_y_interval.width > tolerance) {
      advance
    }
  }
}
