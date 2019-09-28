package ray.shapes

import ray.shapes.Operation.Operation
import ray.tracer.{Intersection, Intersections, Material, Matrix4x4, Point, Ray, Vector}

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

case class Csg(operation: Operation,
               left: Shape, //todo this is children!
               right: Shape,
               transform: Matrix4x4 = Matrix4x4.Identity,
               material: Material = Material(),
               var parent: Shape = null) extends Shape {

  left.parent = this
  right.parent = this

  //todo refactor, replace with polymorphism
  private def includes(a: Shape, b: Shape): Boolean =
    a match {
      case csg: Csg => List(csg.left, csg.right).exists(includes(_, b))
      case g: Group => g.children.exists(includes(_, b))
      case _ => a == b
    }

  override def localIntersect(ray: Ray): Intersections =
    filterIntersections(left.intersect(ray) ::: right.intersect(ray))

  def filterIntersections(intersections: Intersections): Intersections = {
    var inl = false
    var inr = false

    val result = intersections
      .filter(i => {
        val lhit = includes(left, i.obj)
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