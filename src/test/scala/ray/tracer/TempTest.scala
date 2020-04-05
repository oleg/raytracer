package ray.tracer

import org.scalatest.funsuite.AnyFunSuite

class TempTest extends AnyFunSuite {

  val t1 = Matrix4x4.Identity
  val t2 = Matrix4x4.RotationX(0.5)
  val t3 = Matrix4x4.Scaling(0.1, 0.2, 0.3)

  val p = Point(0.4, 0.3, 0.7)
  val expected = Point(4.0, 2.99436, 1.56826)


  case class Box(transform: Matrix4x4, parent: Box) {
    def trans(point: Point): Point = {
      transform.inverse * (if (parent != null) parent.trans(point) else point)
    }
  }

  def transList(point: Point, ts: List[Matrix4x4]): Point = {
    ts.map(_.inverse).fold(Matrix4x4.Identity)(_ * _) * point
  }

  test("local") {
    val res = t3.inverse * t2.inverse * t1.inverse * p

    assert(res ==~ expected, res)
  }

  test("inheritance") {
    val b1 = Box(transform = t1, parent = null)
    val b2 = Box(transform = t2, parent = b1)
    val b3 = Box(transform = t3, parent = b2)

    val res = b3.trans(p)

    assert(res ==~ expected, res)
  }

  test("list of transformations") {
    val res = transList(p, t3 :: t2 :: t1 :: Nil)

    assert(res ==~ expected, res)
  }

  test("world to object on intersection") {
    val xs = Intersection(0.9, null, Double.NaN, Double.NaN, t3 :: t2 :: t1 :: Nil)
    val res = xs.worldToObject(p)
    assert(res ==~ expected, res)
  }
}
