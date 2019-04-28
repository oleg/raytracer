package ray.tracer

import org.scalatest.FunSuite

class TupleTest extends FunSuite {

  test("A tuple equal to another tuple") {
    val t = Tuple(4.3, -4.2, 3.1, 1.0)

    assert(t == Tuple(4.3, -4.2, 3.1, 1.0))
  }

  test("A tuple with w=1.0 is a point") {
    val t = Tuple(4.3, -4.2, 3.1, 1.0)
    assert(t.x == 4.3)
    assert(t.y == -4.2)
    assert(t.z == 3.1)
    assert(t.w == 1.0)
    assert(t.isPoint)
    assert(!t.isVector)
  }

  test("A tuple with w=0 is a vector") {
    val t = Tuple(4.3, -4.2, 3.1, 0.0)
    assert(t.x == 4.3)
    assert(t.y == -4.2)
    assert(t.z == 3.1)
    assert(t.w == 0.0)
    assert(t.isVector)
    assert(!t.isPoint)
  }

  test("point() creates tuples with w=1") {
    val p = Point(4, -4, 3)
    val t = Tuple(4, -4, 3, 1)

    assert(p == t)
    assert(t == p)
  }

  test("vector() creates tuples with w=0") {
    val v = Vector(4, -4, 3)
    val t = Tuple(4, -4, 3, 0)

    assert(v == t)
    assert(t == v)
  }

  test("Adding two tuples") {
    val a1 = Tuple(3, -2, 5, 1)
    val a2 = Tuple(-2, 3, 1, 0)

    assert(a1 + a2 == Tuple(1, 1, 6, 1))
  }

  test("Subtracting two points") {
    val p1 = Point(3, 2, 1)
    val p2 = Point(5, 6, 7)

    assert(p1 - p2 == Vector(-2, -4, -6))
  }

  test("Subtracting a vector from a point") {
    val p = Point(3, 2, 1)
    val v = Vector(5, 6, 7)

    assert(p - v == Point(-2, -4, -6))
  }

  test("Subtracting two vectors") {
    val v1 = Vector(3, 2, 1)
    val v2 = Vector(5, 6, 7)

    assert(v1 - v2 == Vector(-2, -4, -6))
  }

  test("Subtracting a vector from the zero vector") {
    val zero = Vector(0, 0, 0)
    val v = Vector(1, -2, 3)

    assert(zero - v == Vector(-1, 2, -3))
  }

  test("Negating a tuple") {
    val a = Tuple(1, -2, 3, -4)

    assert(-a == Tuple(-1, 2, -3, 4))
  }
  test("Multiplying a tuple by a scalar") {
    val a = Tuple(1, -2, 3, -4)

    assert(a * 3.5 == Tuple(3.5, -7, 10.5, -14))
  }
  test("Multiplying a tuple by a fraction") {
    val a = Tuple(1, -2, 3, -4)

    assert(a * 0.5 == Tuple(0.5, -1, 1.5, -2))
  }

  test("Dividing a tuple by a scalar") {
    val a = Tuple(1, -2, 3, -4)

    assert(a / 2 == Tuple(0.5, -1, 1.5, -2))
  }

  test("Computing the magnitude of vector(1, 0, 0)") {
    val v = Vector(1, 0, 0)
    assert(v.magnitude == 1)
  }
  test("Computing the magnitude of vector(0, 1, 0)") {
    val v = Vector(0, 1, 0)
    assert(v.magnitude == 1)
  }
  test("Computing the magnitude of vector(0, 0, 1)") {
    val v = Vector(0, 0, 1)
    assert(v.magnitude == 1)
  }

  test("Computing the magnitude of vector(1, 2, 3)") {
    val v = Vector(1, 2, 3)
    assert(v.magnitude == math.sqrt(14))
  }
  test("Computing the magnitude of vector(-1, -2, -3)") {
    val v = Vector(-1, -2, -3)
    assert(v.magnitude == math.sqrt(14))
  }

  test("Normalizing vector(4, 0, 0) gives (1, 0, 0)") {
    val v = Vector(4, 0, 0)
    assert(v.normalize == Vector(1, 0, 0))
  }
  test("Normalizing vector(1, 2, 3)") {
    val v = Vector(1, 2, 3)
    //vector(1/√14,   2/√14,   3/√14)​
    assert(v.normalize ==~ Vector(0.26726, 0.53452, 0.80178))
  }

  test("The magnitude of a normalized vector") {
    val v = Vector(1, 2, 3)
    assert(v.normalize.magnitude == 1)
  }

  test("The dot product of two tuples") {
    val a = Vector(1, 2, 3)
    val b = Vector(2, 3, 4)

    assert((a dot b) == 20)
  }

  test("The cross product of two vectors") {
    val a = Vector(1, 2, 3)
    val b = Vector(2, 3, 4)

    assert((a cross b) ==~ Vector(-1, 2, -1))
    assert((b cross a) ==~ Vector(1, -2, 1))
  }

  test("Reflecting a vector approaching at 45 grad") {
    val v = Vector(1, -1, 0)
    val n = Vector(0, 1, 0)

    val r = v.reflect(n)

    assert(r ==~ Vector(1, 1, 0))
  }

  test("Reflecting a vector off a slanted surface") {
    val v = Vector(0, -1, 0)
    val p = math.sqrt(2) / 2
    val n = Vector(p, p, 0)

    val r = v.reflect(n)

    assert(r ==~ Vector(1, 0, 0))
  }

}
