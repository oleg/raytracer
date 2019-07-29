package ray.tracer

case class Ray(origin: Point, direction: Vector) {

  def transform(m: Matrix4x4): Ray = Ray(m * origin, m * direction)

  def position(t: Double): Point = origin + (direction * t)

}
