package org.orbitfold.iafs

import java.lang.ArithmeticException

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

  /** Check if a number is included in the interval */
  def contains(x: Double): Boolean =
    x >= a && x <= b

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

