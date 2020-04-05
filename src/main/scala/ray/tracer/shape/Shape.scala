package ray.tracer.shape

import ray.tracer.{Intersection, Intersections, Material, Matrix4x4, Point, Ray, Vector}

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
