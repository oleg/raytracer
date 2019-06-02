package ray.tracer

trait Shape {

  val transform: Matrix4x4

  val material: Material

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

case class Sphere(transform: Matrix4x4 = Matrix4x4.Identity,
                  material: Material = Material()) extends Shape {

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

object Sphere {

  def glass(transform: Matrix4x4 = Matrix4x4.Identity,
            material: Material = Material(transparency = 1.0, refractiveIndex = 1.5)): Sphere = Sphere(transform, material)

}

case class Plane(transform: Matrix4x4 = Matrix4x4.Identity,
                 material: Material = Material()) extends Shape {

  override def localIntersect(localRay: Ray): Intersections = {
    if (math.abs(localRay.direction.y) < EPSILON) {
      return Intersections(Nil)
    }
    val t = -localRay.origin.y / localRay.direction.y
    Intersections(Intersection(t, this) :: Nil)
  }

  override def localNormalAt(localPoint: Tuple): Tuple = Vector(0, 1, 0)

}

case class Cube(transform: Matrix4x4 = Matrix4x4.Identity,
                material: Material = Material()) extends Shape {

  override def localIntersect(localRay: Ray): Intersections = {
    val (xtmin, xtmax) = checkAxis(localRay.origin.x, localRay.direction.x)
    val (ytmin, ytmax) = checkAxis(localRay.origin.y, localRay.direction.y)
    val (ztmin, ztmax) = checkAxis(localRay.origin.z, localRay.direction.z)

    val tmin = List(xtmin, ytmin, ztmin).max
    val tmax = List(xtmax, ytmax, ztmax).min

    if (tmin > tmax) Intersections(Nil)
    else Intersections(Intersection(tmin, this) :: Intersection(tmax, this) :: Nil)
  }

  private def checkAxis(origin: Double, direction: Double): (Double, Double) = {
    val (tmin, tmax) = ((-1 - origin) / direction, (1 - origin) / direction)
    if (tmin > tmax) (tmax, tmin) else (tmin, tmax)
  }

  override def localNormalAt(localPoint: Tuple): Tuple = {
    val maxc = List(localPoint.x, localPoint.y, localPoint.z).map(math.abs).max

    if (maxc == math.abs(localPoint.x))
      Vector(localPoint.x, 0, 0)
    else if (maxc == math.abs(localPoint.y))
      Vector(0, localPoint.y, 0)
    else
      Vector(0, 0, localPoint.z)
  }

}

