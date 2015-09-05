import org.scalatest.FunSuite
import org.orbitfold.iafs._
import org.orbitfold.iafs.problems._

class ProblemSuite extends FunSuite {
  test("Sphere should have the global minimum at the provided location") {
    val sphere = new Sphere(Vector(new Interval(-2.0), new Interval(0.5), new Interval(11.0)))
    assert(sphere(Vector(new Interval(-2.0), new Interval(0.5), new Interval(11.0)))
      == new Interval(0.0))
  }
}
