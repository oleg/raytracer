package ray

case class Ray(origin: Tuple, direction: Tuple) {

  def transform(m: Matrix4x4): Ray = Ray(m * origin, m * direction)

  def position(t: Double): Tuple = origin + (direction * t)

  def intersect(s: Sphere): Intersections = transform(s.transform.inverse).selfIntersect(s)

  private def selfIntersect(s: Sphere): Intersections = {
    val sphereToRay = origin - Point(0, 0, 0)

    val a: Double = direction dot direction
    val b: Double = 2 * (direction dot sphereToRay)
    val c: Double = (sphereToRay dot sphereToRay) - 1

    val discriminant = b * b - 4 * a * c

    if (discriminant >= 0) {
      val sd = math.sqrt(discriminant)
      val t1 = (-b - sd) / (2 * a)
      val t2 = (-b + sd) / (2 * a)
      Intersections(Intersection(t1, s), Intersection(t2, s))
    } else {
      Intersections()
    }
  }
}
