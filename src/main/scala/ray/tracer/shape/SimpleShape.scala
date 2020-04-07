package ray.tracer.shape

import ray.tracer.shapemath.{Inter, ShapeMath}
import ray.tracer.{Intersection, Intersections, Material, Matrix4x4, Point, Ray, Vector}

case class SimpleShape(math: ShapeMath,
                       transform: Matrix4x4 = Matrix4x4.Identity,
                       material: Material = Material()) extends Shape {

  override def localIntersect(ray: Ray): Intersections =
    Intersections(math.intersect(ray).map(i => Intersection(i.t, this, Nil, i.u, i.v)))

}
