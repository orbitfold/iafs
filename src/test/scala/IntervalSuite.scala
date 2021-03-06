import org.scalatest.FunSuite
import org.orbitfold.iafs._

class IntervalSuite extends FunSuite {
  test("interval should swap bounds if a is larger than b") {
    val interval = new Interval(0.5, 0.2)
    assert(interval.a == 0.2)
    assert(interval.b == 0.5)
    assert(interval.a < interval.b) 
  }

  test("interval addition should work according to interval arithmetic") {
    val interval1 = new Interval(0.1, 0.2)
    val interval2 = new Interval(1.0, 3.0)
    val sum = interval1 + interval2
    assert(sum.a == 1.1)
    assert(sum.b == 3.2)
  }

  test("interval subtraction should work according to interval arithmetic") {
    val interval1 = new Interval(0.1, 0.2)
    val interval2 = new Interval(1.0, 3.0)
    val sub = interval1 - interval2
    assert(sub.a == -2.9)
    assert(sub.b == -0.8)
  }

  test("midpoint should return the midpoint of the interval") {
    val interval = new Interval(0.0, 1.0)
    assert(interval.midpoint == 0.5)
  }

  test("inclusion should return true if a Double falls within the interval bounds") {
    val interval = new Interval(-1.0, 1.0)
    assert(interval.contains(0.0))
    assert(!interval.contains(2.0))
    assert(!interval.contains(-2.0))
  }

  test("interval multiplication should work according to interval arithmetic") {
    val interval1 = new Interval(-2.0, 4.0)
    val interval2 = new Interval(-3.0, -1.0)
    val mul = interval1 * interval2
    assert(mul.a == -12.0)
    assert(mul.b == 6.0)
  }

  test("interval division should work according to interval arithmetic") {
    val interval1 = new Interval(-2.0, 4.0)
    val interval2 = new Interval(-3.0, -1.0)
    intercept[ArithmeticException] {
      interval2 / interval1
    }
    val div = interval1 / interval2
    assert(div.a == -4.0)
    assert(div.b == 2.0)
  }

  test("equality operator should work according to interval arithmetic") {
    val interval1 = new Interval(-2.0, 4.0)
    assert(interval1 == interval1)
  }

  test("inequality operators should work according to interval arithmetic") {
    val interval1 = new Interval(-2.0, 4.0)
    val interval2 = new Interval(5.0, 6.0)
    assert(interval1 < interval2)
    assert(interval2 > interval1)
    assert(interval1 != interval2)
  }

  test("width should return the width of the interval") {
    val interval = new Interval(-2.0, 2.0)
    assert(interval.width == 4.0)
  }

  test("mag should return the magnitude of the interval") {
    val interval = new Interval(-3.0, 2.0)
    assert(interval.mag == 3.0)
  }

  test("mig should return the mignitude of the interval") {
    val interval = new Interval(-3.0, -1.0)
    assert(interval.mig == 1.0)
  }

  test("contains should show if an interval is contained in another interval") {
    val interval1 = new Interval(-3.0, -1.0)
    val interval2 = new Interval(-2.0, -1.0)
    val interval3 = new Interval(-4.0, -1.0)
    val interval4 = new Interval(-2.0, 1.0)
    assert(interval1.contains(interval2))
    assert(!interval1.contains(interval3))
    assert(!interval1.contains(interval4))
  }

  test("all basic interval arithmetic operations should be inclusion isotonic") {
    val interval1 = new Interval(2.0, 4.0)
    val interval2 = new Interval(2.5, 3.5)
    val interval3 = new Interval(1.0, 3.0)
    val interval4 = new Interval(1.5, 2.5)
    assert((interval1 + interval3).contains(interval2 + interval3))
    assert((interval1 - interval3).contains(interval2 - interval3))
    assert((interval1 * interval3).contains(interval2 * interval3))
    assert((interval1 / interval3).contains(interval2 / interval3))
  }

  test("square root of the interval should work correctly") {
    val interval = new Interval(4.0, 16.0)
    val sqrt_interval = Interval.sqrt(interval)
    assert(sqrt_interval.a == 2.0)
    assert(sqrt_interval.b == 4.0)
  }

  test("square root should raise an exception if used on intervals containing negative numbers") {
    val interval = new Interval(-2.0, 16.0)
    intercept[ArithmeticException] {
      Interval.sqrt(interval)
    }
  }
}
