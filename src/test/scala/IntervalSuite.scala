import org.scalatest.FunSuite
import org.orbitfold.iafs._

class IntervalSuite extends FunSuite {
  test("Interval should swap bounds if a is larger than b") {
    val interval = new Interval(0.5, 0.2)
    assert(interval.a == 0.2)
    assert(interval.b == 0.5)
    assert(interval.a < interval.b) 
  }

  test("Interval addition should work according to interval arithmetic") {
    val interval1 = new Interval(0.1, 0.2)
    val interval2 = new Interval(1.0, 3.0)
    val sum = interval1 + interval2
    assert(sum.a == 1.1)
    assert(sum.b == 3.2)
  }

  test("Interval subtraction should work according to interval arithmetic") {
    val interval1 = new Interval(0.1, 0.2)
    val interval2 = new Interval(1.0, 3.0)
    val sub = interval1 - interval2
    assert(sub.a == -2.9)
    assert(sub.b == -0.8)
  }

  test("Midpoint should return the midpoint of the interval") {
    val interval = new Interval(0.0, 1.0)
    assert(interval.midpoint == 0.5)
  }

  test("Inclusion should return true if a Double falls within the interval bounds") {
    val interval = new Interval(-1.0, 1.0)
    assert(interval.contains(0.0))
    assert(!interval.contains(2.0))
    assert(!interval.contains(-2.0))
  }

  test("Interval multiplication should work according to interval arithmetic") {
    val interval1 = new Interval(-2.0, 4.0)
    val interval2 = new Interval(-3.0, -1.0)
    val mul = interval1 * interval2
    assert(mul.a == -12.0)
    assert(mul.b == 6.0)
  }

  test("Interval division should work according to interval arithmetic") {
    val interval1 = new Interval(-2.0, 4.0)
    val interval2 = new Interval(-3.0, -1.0)
    intercept[ArithmeticException] {
      interval2 / interval1
    }
    val div = interval1 / interval2
    assert(div.a == -4.0)
    assert(div.b == 2.0)
  }

  test("Equality operator should work according to interval arithmetic") {
    val interval1 = new Interval(-2.0, 4.0)
    assert(interval1 == interval1)
  }

  test("Inequality operators should work according to interval arithmetic") {
    val interval1 = new Interval(-2.0, 4.0)
    val interval2 = new Interval(5.0, 6.0)
    assert(interval1 < interval2)
    assert(interval2 > interval1)
    assert(interval1 != interval2)
  }
}
