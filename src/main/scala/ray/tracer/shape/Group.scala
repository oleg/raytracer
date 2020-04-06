package ray.tracer.shape

import ray.tracer.{Intersection, Intersections, Material, Matrix4x4, Point, Ray, Vector}

import scala.collection.mutable.ListBuffer

//todo implement as immutable, with builder
case class Group(children: ListBuffer[Shape] = ListBuffer(),
                 transform: Matrix4x4 = Matrix4x4.Identity,
                 material: Material = Material()) extends Shape {

  override def localIntersect(ray: Ray): Intersections =
    children.foldLeft(Intersections(Nil))((acc, s) => acc ::: s.intersect(ray))


  override def localNormalAt(point: Point, intersection: Intersection): Vector = ???

  def add(shape: Shape): Unit =
    children += shape

  override def toString: String = s"children: ${children.length}" //todo fix me

  override def incl(b: Shape): Boolean =
    children.exists(_.incl(b))
}
