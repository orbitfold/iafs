package org.orbitfold.iafs.problems

import org.orbitfold.iafs.Interval
import org.orbitfold.iafs.Problem

class Sphere(offsetsc: Vector[Interval]) extends Problem {
  val offsets: Vector[Interval] = offsetsc

  def apply(xs: Vector[Interval]) : Interval = {
    val values = for {
      (x, offset) <- xs zip offsets
    } yield (x - offset) * (x - offset) 
    values.foldLeft(new Interval(0.0))(_ + _)
  }
}
