package ray.shapes

import ray.tracer.{Intersection, Intersections, Material, Matrix4x4, Point, Ray, Vector}

trait Shape {
  val transform: Matrix4x4
  val material: Material
  var parent: Shape //todo should not return Group
  def localIntersect(ray: Ray): Intersections
  def localNormalAt(point: Point, intersection: Intersection): Vector

  def intersect(worldRay: Ray): Intersections =
    localIntersect(
      worldRay.transform(transform.inverse))

  def normalAt(worldPoint: Point, intersection: Intersection): Vector =
    normalToWorld(
      localNormalAt(
        worldToObject(worldPoint), intersection))

  def worldToObject(point: Point): Point =
    transform.inverse * (if (parent != null) parent.worldToObject(point) else point)

  def normalToWorld(normal: Vector): Vector = {
    val n = (transform.inverse.transpose * normal).normalize
    if (parent != null) parent.normalToWorld(n) else n
  }

}
