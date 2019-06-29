package ray.tracer

trait Shape {

  val transform: Matrix4x4

  val material: Material

  def localIntersect(ray: Ray): Intersections

  def localNormalAt(point: Tuple): Tuple //todo Point, Vector

  def intersect(worldRay: Ray): Intersections = {
    val ray = worldRay.transform(this.transform.inverse)
    localIntersect(ray)
  }

  //todo how to make accept only points?
  def normalAt(worldPoint: Tuple): Tuple = {
    val point = transform.inverse * worldPoint
    val localNormal = localNormalAt(point)
    val worldNormal = transform.inverse.transpose * localNormal
    worldNormal.toVector.normalize
  }

}

case class Sphere(transform: Matrix4x4 = Matrix4x4.Identity,
                  material: Material = Material()) extends Shape {

  override def localIntersect(ray: Ray): Intersections = {
    val sphereToRay = ray.origin - Point(0, 0, 0)

    val a: Double = ray.direction dot ray.direction
    val b: Double = 2 * (ray.direction dot sphereToRay)
    val c: Double = (sphereToRay dot sphereToRay) - 1

    val discriminant = b * b - 4 * a * c

    if (discriminant >= 0) {
      val sd = math.sqrt(discriminant)
      val t1 = (-b - sd) / (2 * a)
      val t2 = (-b + sd) / (2 * a)
      Intersections(Intersection(t1, this) :: Intersection(t2, this) :: Nil)
    } else {
      Intersections.EMPTY
    }

  }

  override def localNormalAt(point: Tuple): Tuple = point - Point(0, 0, 0)

}

object Sphere {

  def glass(transform: Matrix4x4 = Matrix4x4.Identity,
            material: Material = Material(transparency = 1.0, refractiveIndex = 1.5)): Sphere = Sphere(transform, material)

}

case class Plane(transform: Matrix4x4 = Matrix4x4.Identity,
                 material: Material = Material()) extends Shape {

  override def localIntersect(ray: Ray): Intersections = {
    if (approximatelyEqual(ray.direction.y, 0.0)) {
      Intersections.EMPTY
    } else {
      val t = -ray.origin.y / ray.direction.y
      Intersections(Intersection(t, this) :: Nil)
    }
  }

  override def localNormalAt(point: Tuple): Tuple = Vector(0, 1, 0)

}

case class Cube(transform: Matrix4x4 = Matrix4x4.Identity,
                material: Material = Material()) extends Shape {

  override def localIntersect(ray: Ray): Intersections = {
    val (xtmin, xtmax) = checkAxis(ray.origin.x, ray.direction.x)
    val (ytmin, ytmax) = checkAxis(ray.origin.y, ray.direction.y)
    val (ztmin, ztmax) = checkAxis(ray.origin.z, ray.direction.z)

    val tmin = List(xtmin, ytmin, ztmin).max
    val tmax = List(xtmax, ytmax, ztmax).min

    if (tmin > tmax) Intersections(Nil)
    else Intersections(Intersection(tmin, this) :: Intersection(tmax, this) :: Nil)
  }

  private def checkAxis(origin: Double, direction: Double): (Double, Double) = {
    val (tmin, tmax) = ((-1 - origin) / direction, (1 - origin) / direction)
    if (tmin > tmax) (tmax, tmin) else (tmin, tmax)
  }

  override def localNormalAt(point: Tuple): Tuple = {
    val maxc = List(point.x, point.y, point.z).map(math.abs).max

    if (maxc == point.x.abs)
      Vector(point.x, 0, 0)
    else if (maxc == point.y.abs)
      Vector(0, point.y, 0)
    else
      Vector(0, 0, point.z)
  }

}

case class Cylinder(minimum: Double = Double.NegativeInfinity,
                    maximum: Double = Double.PositiveInfinity,
                    transform: Matrix4x4 = Matrix4x4.Identity,
                    material: Material = Material()) extends Shape {

  override def localIntersect(ray: Ray): Intersections = {
    val fromX = ray.origin.x
    val toX = ray.direction.x
    val fromZ = ray.origin.z
    val toZ = ray.direction.z

    val a = toX * toX + toZ * toZ
    if (approximatelyEqual(a, 0.0)) {
      return Intersections.EMPTY
    }

    val b = 2 * fromX * toX + 2 * fromZ * toZ
    val c = fromX * fromX + fromZ * fromZ - 1
    val discriminant = b * b - 4 * a * c
    if (discriminant < 0) {
      return Intersections.EMPTY
    }

    //TODO implement new math object for solving this
    val t0 = (-b - math.sqrt(discriminant)) / (2 * a)
    val t1 = (-b + math.sqrt(discriminant)) / (2 * a)

    val xs = List(t0, t1)
      .sorted
      .map(t => (t, ray.origin.y + t * ray.direction.y))
      .filter(t2y => minimum < t2y._2 && t2y._2 < maximum)
      .map(t2y => Intersection(t2y._1, this))

    Intersections(xs)
  }

  override def localNormalAt(point: Tuple): Tuple = {
    Vector(point.x, 0, point.z)
  }

}

