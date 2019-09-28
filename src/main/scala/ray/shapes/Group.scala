package ray.shapes

import ray.tracer.{Intersection, Intersections, Material, Matrix4x4, Point, Ray, Vector}

import scala.collection.mutable.ListBuffer

//todo implement as immutable, with builder
case class Group(children: ListBuffer[Shape] = ListBuffer(),
                 transform: Matrix4x4 = Matrix4x4.Identity,
                 material: Material = Material(),
                 parent: Shape = null) extends Shape {

  override def localIntersect(ray: Ray): Intersections =
    children.foldLeft(Intersections(Nil))((acc, s) => acc ::: s.intersect(ray))

  override def localNormalAt(point: Point, intersection: Intersection): Vector =
    ???

  override def newChildrenAdded(shape: Shape): Unit =
    children += shape

  def contains(shape: Shape): Boolean =
    children.contains(shape)

  def size: Int =
    children.length

  override def toString: String =
    s"children: ${children.length}, parent: $parent" //todo fix me
}

