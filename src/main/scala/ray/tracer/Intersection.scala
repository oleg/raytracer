package ray.tracer

import ray.tracer.shape.{Shape, SimpleShape}
import ray.tracer.shapemath.Inter

import scala.collection.mutable.ArrayBuffer

//todo oleg use material instead of obj
case class Computation(t: Double,
                       obj: Shape,
                       point: Point,
                       overPoint: Point,
                       underPoint: Point,
                       eyev: Vector,
                       normalv: Vector,
                       reflectv: Vector,
                       n1: Double,
                       n2: Double,
                       inside: Boolean,
                       objPoint: Point) {

  def schlick(): Double = {
    var cos = eyev dot normalv
    if (n1 > n2) {
      val n = n1 / n2
      val sin2t = n * n * (1.0 - cos * cos)
      if (sin2t > 1.0) {
        return 1.0
      }
      val cosT = math.sqrt(1.0 - sin2t)
      cos = cosT
    }
    val r0 = math.pow((n1 - n2) / (n1 + n2), 2)
    r0 + (1 - r0) * math.pow(1 - cos, 5)
  }

}

case class Intersection(t: Double,
                        obj: SimpleShape,
                        ts: List[Matrix4x4] = Nil,
                        u: Double = Double.NaN,
                        v: Double = Double.NaN) {

  override def toString: String =
    s"""
       |Intersection(
       |$t, $u, $v
       |$obj)
    """.stripMargin

  def prepareComputations(ray: Ray, ns: (Double, Double)): Computation = {
    val p = summon[Precision[Double]]
    val point = ray.position(t)
    val eyev = -ray.direction
    val normalv = myNormalAt(obj, point)
    val inside = (normalv dot eyev) < 0
    val directedNormalv = if (inside) -normalv else normalv
    val overPoint = point + directedNormalv * p.precision
    val underPoint = point - directedNormalv * p.precision
    val reflectv = ray.direction.reflect(directedNormalv)

    val objPoint = worldToObject(overPoint) //todo invoked two times
    Computation(t, obj, point, overPoint, underPoint, eyev, directedNormalv, reflectv, ns._1, ns._2, inside, objPoint)
  }

  def myNormalAt(obj: SimpleShape, worldPoint: Point): Vector = {
    val localPoint: Point = worldToObject(worldPoint)
    val localNormal: Vector = obj.math.normalAt(localPoint, Inter(t, u, v))
    normalToWorld(localNormal)
  }

  //private?
  def worldToObject(point: Point): Point =
    ts.reverse.map(_.inverse).foldLeft(Matrix4x4.Identity)(_ * _) * point

  //private?
  def normalToWorld(nr: Vector): Vector =
    ts.reverse.foldLeft(nr)((acc, el) => (el.inverse.transpose * acc).normalize)

}


object Intersections {
  private val vectorOrdering = Ordering.by((_: Intersection).t)

  def apply(is: List[Intersection]): Intersections = new Intersections(is.sorted(vectorOrdering))
}


case class Intersections private(private val is: List[Intersection]) extends Iterable[Intersection] {

  def apply(i: Int): Intersection =
    is(i)

  def length: Int =
    is.length

  def hit: Option[Intersection] =
    is.find(_.t >= 0)

  def :::(other: Intersections): Intersections =
    Intersections(is ::: other.is)

  override def iterator: Iterator[Intersection] =
    is.iterator

  def findNs(inter: Intersection): (Double, Double) = {
    var n1: Double = 0.0
    var n2: Double = 0.0

    val containners = ArrayBuffer[Shape]()
    for (i <- is) {

      if (i == inter) {
        if (containners.isEmpty) {
          n1 = 1.0
        } else {
          n1 = containners.last.material.refractiveIndex
        }
      }

      if (containners.contains(i.obj)) {
        containners -= i.obj
      } else {
        containners += i.obj
      }

      if (i == inter) {
        if (containners.isEmpty) {
          n2 = 1.0
        } else {
          n2 = containners.last.material.refractiveIndex
        }
        return (n1, n2)
      }
    }
    (n1, n2)
  }

}
