package ray.tracer

import org.scalatest.funsuite.AnyFunSuite
import ray.tracer.Matrix4x4.Identity

class IntersectionNormalToWorldTest extends AnyFunSuite {

  val t1 = Identity.translate(-2, 0, 0).scale(22, 0.9, 1) //group
  val t2 = Identity.scale(0.1, 0.2, 0.3).rotateY(math.Pi) //group1
  val t3 = Identity.rotateX(-math.Pi).shear(1, 2, 3, 4, 5, 6) //shape
  val n = Vector(1.4, 2.3, 0.7)

  val expected = Vector(0.1759620757100133, 0.9843760122638662, 0.006419843558243035)


  case class Box(transform: Matrix4x4, parent: Box) {
    def normalToWorld(normal: Vector): Vector = {
      val n = (transform.inverse.transpose * normal).normalize
      if (parent != null) parent.normalToWorld(n) else n
    }
  }

  test("local") {
    val r1 = (t3.inverse.transpose * n).normalize
    val r2 = (t2.inverse.transpose * r1).normalize
    val res = (t1.inverse.transpose * r2).normalize

    assert(res ==~ expected, res)
  }

  test("inheritance") {
    val g = Box(transform = t1, parent = null)
    val g1 = Box(transform = t2, parent = g)
    val s = Box(transform = t3, parent = g1)

    val res = s.normalToWorld(n)

    assert(res ==~ expected, res)
  }

  test("list of transformations") {
    def normalToWorldList(nr: Vector, ts: List[Matrix4x4]): Vector =
      ts.reverse.foldLeft(nr)((acc, el) => (el.inverse.transpose * acc).normalize)

    val res = normalToWorldList(n, t1 :: t2 :: t3 :: Nil)

    assert(res ==~ expected, res)
  }

  test("normalToWorld on intersection") {
    val xs = Intersection(0.9, null, t1 :: t2 :: t3 :: Nil)
    val res = xs.normalToWorld(n)
    assert(res ==~ expected, res)
  }


  //  test("list of reverse transformations") {
//    def normalToWorldList(nr: Vector, ts: List[Matrix4x4]): Vector =
//      ts.foldLeft(nr)((acc, el) => (el.inverse.transpose * acc).normalize)
//
//
//    val ls = List(t1, t2, t3)
//    val res = normalToWorldList(n, ls)
//    val resRev = normalToWorldList(n, ls.reverse)
//
//    assert(res != resRev, s"res: $res, resRev: $resRev")
//  }


}
