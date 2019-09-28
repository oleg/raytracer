package ray.shapes

import ray.tracer.{Intersection, Intersections, Material, Matrix4x4, Point, Precision, Ray, Vector}

case class Plane(transform: Matrix4x4 = Matrix4x4.Identity,
                 material: Material = Material(),
                 var parent: Shape = null) extends Shape { //todo can I make it immutable

  //todo:oleg can I simplify this, should method expect implicit param?
  override def localIntersect(ray: Ray): Intersections =
    if (implicitly[Precision[Double]].approximatelyEqual(ray.direction.y, 0.0)) {
      Intersections.EMPTY
    } else {
      val t = -ray.origin.y / ray.direction.y
      Intersections(Intersection(t, this) :: Nil)
    }

  override def localNormalAt(point: Point, intersection: Intersection): Vector =
    Vector(0, 1, 0)

}
