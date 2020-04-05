package ray.tracer

import ray.tracer.Operation.Operation
import ray.tracer.shapemath._

import scala.collection.mutable.ListBuffer


trait Shape {

  val transform: Matrix4x4
  val material: Material
  var parent: Shape //todo should not return Group

  def intersect(worldRay: Ray): Intersections = {
    val ray = worldRay.transform(transform.inverse)
    localIntersect(ray)
  }

  def localIntersect(ray: Ray): Intersections

  def normalAt(worldPoint: Point, intersection: Intersection): Vector = {
    val localPoint: Point = worldToObject(worldPoint)
    val localNormal: Vector = localNormalAt(localPoint, intersection)
    normalToWorld(localNormal)
  }

  def localNormalAt(point: Point, intersection: Intersection): Vector


  def worldToObject(point: Point): Point = {
    transform.inverse * (if (parent != null) parent.worldToObject(point) else point)
  }

  def normalToWorld(normal: Vector): Vector = {
    val n = (transform.inverse.transpose * normal).normalize
    if (parent != null) parent.normalToWorld(n) else n
  }

  def incl(b: Shape): Boolean = this == b

}

case class SimpleShape(math: ShapeMath,
                       transform: Matrix4x4 = Matrix4x4.Identity,
                       material: Material = Material(),
                       var parent: Shape = null) extends Shape {

  override def localIntersect(ray: Ray): Intersections =
    Intersections(math.intersect(ray).map(i => Intersection(i.t, this, i.u, i.v)))

  override def localNormalAt(point: Point, intersection: Intersection): Vector =
    math.normalAt(point, Option(intersection).map(i => Inter(i.t, i.u, i.v)).orNull)

}

//todo implement as immutable, with builder
case class Group(children: ListBuffer[Shape] = ListBuffer(),
                 transform: Matrix4x4 = Matrix4x4.Identity,
                 material: Material = Material(),
                 var parent: Shape = null) extends Shape {

  override def localIntersect(ray: Ray): Intersections =
    children.foldLeft(Intersections(Nil))((acc, s) => acc ::: s.intersect(ray))


  override def localNormalAt(point: Point, intersection: Intersection): Vector = ???

  def add(shape: Shape): Unit = {
    children += shape
    shape.parent = this //todo refactor
  }

  override def toString: String = s"children: ${children.length}, parent: $parent" //todo fix me

  override def incl(b: Shape): Boolean =
    children.exists(_.incl(b))
}

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