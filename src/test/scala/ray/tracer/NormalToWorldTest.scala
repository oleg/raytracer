package ray.tracer

import org.scalatest.funsuite.AnyFunSuite

class NormalToWorldTest extends AnyFunSuite {

  val t1 = Matrix4x4.Identity
  val t2 = Matrix4x4.RotationX(0.5)
  val t3 = Matrix4x4.Scaling(0.1, 0.2, 0.3)

  val n = Vector(0.4, 0.3, 0.7)
  val expected = Vector(0.82174, 0.04061, 0.56840)


  case class Box(transform: Matrix4x4, parent: Box) {
    def normalToWorld(normal: Vector): Vector = {
      val n = (transform.inverse.transpose * normal).normalize
      if (parent != null) parent.normalToWorld(n) else n
    }
  }

  def normalToWorldList(nr: Vector, ts: List[Matrix4x4]): Vector =
    ts.foldLeft(nr)((acc, el) => (el.inverse.transpose * acc).normalize)

  test("local") {
    val r1 = (t3.inverse.transpose * n).normalize
    val r2 = (t2.inverse.transpose * r1).normalize
    val res = (t1.inverse.transpose * r2).normalize

    assert(res ==~ expected, res)
  }

  test("inheritance") {
    val b1 = Box(transform = t1, parent = null)
    val b2 = Box(transform = t2, parent = b1)
    val b3 = Box(transform = t3, parent = b2)

    val res = b3.normalToWorld(n)

    assert(res ==~ expected, res)
  }

  test("list of transformations") {
    val res = normalToWorldList(n, t3 :: t2 :: t1 :: Nil)

    assert(res ==~ expected, res)
  }

  test("normalToWorld on intersection") {
    val xs = Intersection(0.9, null, Double.NaN, Double.NaN, t3 :: t2 :: t1 :: Nil)
    val res = xs.normalToWorld(n)
    assert(res ==~ expected, res)
  }
}
