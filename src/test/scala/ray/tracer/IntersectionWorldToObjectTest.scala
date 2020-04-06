package ray.tracer

import org.scalatest.funsuite.AnyFunSuite
import ray.tracer.Matrix4x4.Identity

class IntersectionWorldToObjectTest extends AnyFunSuite {

  val t1 = Identity.translate(-2, 0, 0) //group
  val t2 = Identity.scale(0.5, 0.5, 0.5) //group1
  val t3 = Identity.translate(-1.5, 1, 1.3) //shape

  val p = Point(-3.25, 0.9530619270774618, 0.17617323333782092)
  val expected = Point(-1.0, 0.9061238541549237, -0.9476535333243582)


  case class Box(transform: Matrix4x4, parent: Box) {
    def worldToObject(point: Point): Point = {
      transform.inverse * (if (parent != null) parent.worldToObject(point) else point)
    }
  }

  test("local") {
    val res = t3.inverse * t2.inverse * t1.inverse * p

    assert(res ==~ expected, res)
  }

  test("inheritance") {
    val g = Box(transform = t1, parent = null)
    val b1 = Box(transform = t2, parent = g)
    val s = Box(transform = t3, parent = b1)

    val res = s.worldToObject(p)

    assert(res ==~ expected, res)
  }

  test("list of transformations") {
    def worldToObjectList(point: Point, ts: List[Matrix4x4]): Point = {
      ts.reverse.map(_.inverse).fold(Matrix4x4.Identity)(_ * _) * point
    }

    val res = worldToObjectList(p, t1 :: t2 :: t3 :: Nil)

    assert(res ==~ expected, res)
  }

  test("world to object on intersection") {
    val xs = Intersection(0.9, null, t1 :: t2 :: t3 :: Nil)
    val res = xs.worldToObject(p)
    assert(res ==~ expected, res)
  }
}
