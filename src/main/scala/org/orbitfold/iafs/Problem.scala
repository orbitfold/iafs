package org.orbitfold.iafs

trait Problem {
  def apply(xs: Vector[Interval]) : Interval 
}
