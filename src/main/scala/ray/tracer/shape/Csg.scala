package ray.tracer.shape

import ray.tracer.shape.Operation.Operation
import ray.tracer.{Intersection, Intersections, Material, Matrix4x4, Point, Ray, Vector}

case class Csg(operation: Operation,
               left: Shape,
               right: Shape,
               transform: Matrix4x4 = Matrix4x4.Identity,
               material: Material = Material(),
               var parent: Shape = null) extends Shape {

  left.parent = this
  right.parent = this

  override def incl(b: Shape): Boolean =
    left.incl(b) || right.incl(b)

  override def localIntersect(ray: Ray): Intersections =
    filterIntersections(left.intersect(ray) ::: right.intersect(ray))

  def filterIntersections(intersections: Intersections): Intersections = {
    var inl = false
    var inr = false

    val result = intersections
      .filter(i => {
        val lhit = left.incl(i.obj)
        val allowed = Operation.intersectionAllowed(operation, lhit, inl, inr)
        if (lhit) {
          inl = !inl
        } else {
          inr = !inr
        }
        allowed
      })
    Intersections(result.toList)
  }

  override def localNormalAt(point: Point, hit: Intersection): Vector = ???

  override def toString: String = s"CSG($operation, left, right)"
}

object Operation extends Enumeration {
  type Operation = Value
  val union, intersection, difference = Value

  //replace with case classes?
  def intersectionAllowed(op: Operation, lhit: Boolean, inl: Boolean, inr: Boolean): Boolean =
    op match {
      case `union` => (lhit && !inr) || (!lhit && !inl)
      case `intersection` => (lhit && inr) || (!lhit && inl)
      case `difference` => (lhit && !inr) || (!lhit && inl)
    }
}
