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
}
