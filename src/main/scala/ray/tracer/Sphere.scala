package ray.tracer

trait Shape {

  val transform: Matrix4x4

  def localIntersect(localRay: Ray): Intersections

  def localNormalAt(localPoint: Tuple): Tuple //todo Point, Vector

  def intersect(ray: Ray): Intersections = {
    val localRay = ray.transform(this.transform.inverse)
    localIntersect(localRay)
  }

  //todo how to make accept only points?
  def normalAt(worldPoint: Tuple): Tuple = {
    val localPoint = transform.inverse * worldPoint
    val localNormal = localNormalAt(localPoint)
    val worldNormal = transform.inverse.transpose * localNormal
    worldNormal.toVector.normalize
  }

}

case class Sphere(transform: Matrix4x4 = Matrix4x4.Identity, material: Material = Material()) extends Shape {

  override def localIntersect(localRay: Ray): Intersections = {
    val sphereToRay = localRay.origin - Point(0, 0, 0)

    val a: Double = localRay.direction dot localRay.direction
    val b: Double = 2 * (localRay.direction dot sphereToRay)
    val c: Double = (sphereToRay dot sphereToRay) - 1

    val discriminant = b * b - 4 * a * c

    if (discriminant >= 0) {
      val sd = math.sqrt(discriminant)
      val t1 = (-b - sd) / (2 * a)
      val t2 = (-b + sd) / (2 * a)
      Intersections(Intersection(t1, this) :: Intersection(t2, this) :: Nil)
    } else {
      Intersections(Nil)
    }

  }

  override def localNormalAt(localPoint: Tuple): Tuple = localPoint - Point(0, 0, 0)

}
