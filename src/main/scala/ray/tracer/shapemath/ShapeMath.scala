package ray.tracer.shapemath

import ray.tracer.{Point, Ray, Vector}

//todo find a better name
case class Inter(t: Double,
                 u: Double = Double.NaN,
                 v: Double = Double.NaN)

trait ShapeMath {
  def intersect(ray: Ray): List[Inter]

  def normalAt(point: Point, inter: Inter): Vector
}