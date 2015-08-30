package org.orbitfold.iafs

import java.lang.ArithmeticException
import scala.math.max
import scala.math.abs

class Interval(ac: Double, bc: Double) {
  var a: Double = ac
  var b: Double = bc
  if (a > b) {
    val tmp = b
    b = a
    a = tmp
  }

  /** Return the midpoint of the interval */
  def midpoint =
    (a + b) * 0.5

  /** Return the width of the interval */
  def width =
    b - a

  /** Return magnitude of the interval */
  def mag =
    max(abs(a), abs(b))

  /** Return mignitude of the interval */
  def mig = 
    if (a > 0) {
      a
    } else if (b < 0) {
      -b
    } else {
      0
    }

  /** Check if a number is included in the interval */
  def contains(x: Double): Boolean =
    x >= a && x <= b

  /** Check if a given interval is contained in this interval */
  def contains(x: Interval): Boolean =
    x.a >= a && x.b <= b

  def +(that: Interval): Interval =
    new Interval(this.a + that.a, this.b + that.b)

  def -(that: Interval): Interval =
    new Interval(this.a - that.b, this.b - that.a)

  def *(that: Interval): Interval = {
    val comb = List(this.a * that.a, this.a * that.b, this.b * that.a, this.b * that.b)
    new Interval(comb.min, comb.max)
  }

  def /(that: Interval): Interval = {
    if (that.contains(0.0))
      throw new ArithmeticException("Division by an interval containing zero")
    val comb = List(this.a / that.a, this.a / that.b, this.b / that.a, this.b / that.b)
    new Interval(comb.min, comb.max)
  }

  def ==(that: Interval): Boolean = {
    this.a == that.a && this.b == that.b
  }

  def <(that: Interval): Boolean = {
    this.b < that.a
  }

  def >(that: Interval): Boolean = {
    that.b < this.a
  }

  override def toString(): String =
    "[" + a + ", " + b + "]"
}

