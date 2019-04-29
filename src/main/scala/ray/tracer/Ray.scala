package ray.tracer

case class Ray(origin: Tuple, direction: Tuple) {

  def transform(m: Matrix4x4): Ray = Ray(m * origin, m * direction)

  def position(t: Double): Tuple = origin + (direction * t)

}
