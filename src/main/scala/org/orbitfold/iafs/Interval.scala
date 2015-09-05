package org.orbitfold.iafs

import java.lang.ArithmeticException
import scala.math.{max, abs, Pi, floor}
import scala.math.{sqrt => sqrt_, sin => sin_, cos => cos_}

object Interval {
  def sqrt(x: Interval): Interval = {
    if (x.a < 0.0)
      throw new ArithmeticException("Square root of an interval including negative numbers is undefined")
    new Interval(sqrt_(x.a), sqrt_(x.b))
  }

/**
  def sin(x: Interval): Interval = {
    if (x.width >= 2.0 * Pi) {
      new Interval(-1.0, 1.0)
    } else {
      val n = floor(x.a / (2.0 * Pi))
      val a = x.a - n * 2.0 * Pi
      val b = x.b - n * 2.0 * Pi
      val x_interval = new Interval(a, b)
      if (x_interval.contains(0.5 * Pi)) {
        if (x_interval.contains(1.5 * Pi)) {
          new Interval(-1.0, 1.0)
        } else {
          val sin_a = sin_(a)
          val sin_b = sin_(b)

        }
      }
    }
  }

  def cos(x: Interval): Interval = {
    if (x.width >= 2.0 * Pi) {
      new Interval(-1.0, 1.0)
    }
  }
  */
}

class Interval(ac: Double, bc: Double) {
  var a: Double = ac
  var b: Double = bc
  if (a > b) {
    val tmp = b
    b = a
    a = tmp
  }

  /** Construct degenerate interval */
  def this(x: Double) = this(x, x)

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

