package ray.tracer.shape

import ray.tracer.{Intersection, Intersections, Material, Matrix4x4, Point, Ray, Vector}

trait Shape {

  val transform: Matrix4x4
  //only for SimpleShape
  val material: Material

  def intersect(worldRay: Ray): Intersections = {
    val ray = worldRay.transform(transform.inverse)
    val xs = localIntersect(ray)
    Intersections(xs.map(i => Intersection(i.t, i.obj, transform :: i.ts, i.u, i.v)).toList)
  }

  def localIntersect(ray: Ray): Intersections

  //only for SimpleShape
  def localNormalAt(point: Point, intersection: Intersection): Vector

  def incl(b: Shape): Boolean = this == b

}
