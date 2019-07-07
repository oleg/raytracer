package ray.tracer

import ray.tracer.Operation.Operation

import scala.collection.mutable.ListBuffer


trait Shape {

  val transform: Matrix4x4

  val material: Material

  var parent: Shape //todo should not return Group

  def localIntersect(ray: Ray): Intersections

  def localNormalAt(point: Tuple, intersection: Intersection): Tuple //todo Point, Vector

  def intersect(worldRay: Ray): Intersections = {
    val ray = worldRay.transform(transform.inverse)
    localIntersect(ray)
  }

  //todo how to make accept only points?
  def normalAt(worldPoint: Tuple, intersection: Intersection): Tuple = {
    val localPoint = worldToObject(worldPoint)
    val localNormal = localNormalAt(localPoint, intersection)
    normalToWorld(localNormal)
  }

  def worldToObject(point: Tuple): Tuple = {
    transform.inverse * (if (parent != null) parent.worldToObject(point) else point)
  }

  def normalToWorld(normal: Tuple /*Vector*/): Tuple = {
    val n = (transform.inverse.transpose * normal).toVector.normalize
    if (parent != null) parent.normalToWorld(n) else n
  }

}

case class Sphere(transform: Matrix4x4 = Matrix4x4.Identity,
                  material: Material = Material(),
                  var parent: Shape = null) extends Shape {

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

  override def localNormalAt(point: Tuple, intersection: Intersection): Tuple = point - Point(0, 0, 0)

}

object Sphere {

  def glass(transform: Matrix4x4 = Matrix4x4.Identity,
            material: Material = Material(transparency = 1.0, refractiveIndex = 1.5)): Sphere = Sphere(transform, material)

}

case class Plane(transform: Matrix4x4 = Matrix4x4.Identity,
                 material: Material = Material(),
                 var parent: Shape = null) extends Shape {

  override def localIntersect(ray: Ray): Intersections = {
    if (approximatelyEqual(ray.direction.y, 0.0)) {
      Intersections.EMPTY
    } else {
      val t = -ray.origin.y / ray.direction.y
      Intersections(Intersection(t, this) :: Nil)
    }
  }

  override def localNormalAt(point: Tuple, intersection: Intersection): Tuple = Vector(0, 1, 0)

}

case class Cube(transform: Matrix4x4 = Matrix4x4.Identity,
                material: Material = Material(),
                var parent: Shape = null) extends Shape {

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

  override def localNormalAt(point: Tuple, intersection: Intersection): Tuple = {
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
                    closed: Boolean = false,
                    transform: Matrix4x4 = Matrix4x4.Identity,
                    material: Material = Material(),
                    var parent: Shape = null) extends Shape {

  override def localIntersect(ray: Ray): Intersections = {
    val a = ray.direction.x * ray.direction.x + ray.direction.z * ray.direction.z
    val b = 2 * ray.origin.x * ray.direction.x + 2 * ray.origin.z * ray.direction.z
    val c = ray.origin.x * ray.origin.x + ray.origin.z * ray.origin.z - 1

    if (approximatelyEqual(a, 0.0)) {
      return Intersections(intersectCaps(ray)) //todo refactor
    }

    val discriminant = b * b - 4 * a * c
    if (discriminant < 0) {
      return Intersections(intersectCaps(ray))
    }

    //TODO implement new math object for solving this
    val t0 = (-b - math.sqrt(discriminant)) / (2 * a)
    val t1 = (-b + math.sqrt(discriminant)) / (2 * a)

    val xsCylinder = List(t0, t1)
      .sorted
      .map(t => (t, ray.origin.y + t * ray.direction.y))
      .filter(t2y => minimum < t2y._2 && t2y._2 < maximum)
      .map(t2y => Intersection(t2y._1, this))

    val xsCaps = intersectCaps(ray)

    Intersections(xsCaps ::: xsCylinder)
  }

  private def intersectCaps(ray: Ray): List[Intersection] = {
    var result: List[Intersection] = Nil //todo refactor

    if (!closed || approximatelyEqual(ray.direction.y, 0.0)) {
      return result
    }

    val t0 = (minimum - ray.origin.y) / ray.direction.y
    if (checkCap(ray, t0)) {
      result ::= Intersection(t0, this)
    }

    val t1 = (maximum - ray.origin.y) / ray.direction.y
    if (checkCap(ray, t1)) {
      result ::= Intersection(t1, this)
    }

    result
  }

  private def checkCap(ray: Ray, t: Double): Boolean = {
    val x = ray.origin.x + t * ray.direction.x
    val z = ray.origin.z + t * ray.direction.z
    (x * x + z * z) <= 1
  }

  override def localNormalAt(point: Tuple, intersection: Intersection): Tuple = {
    val dist = point.x * point.x + point.z * point.z

    if (dist < 1 && (point.y >= maximum - EPSILON)) { //todo remove reference to epsilon
      Vector(0, 1, 0)
    } else if (dist < 1 && (point.y <= minimum + EPSILON)) {
      Vector(0, -1, 0)
    } else {
      Vector(point.x, 0, point.z)
    }
  }
}

//todo refactor this class it's similar to Cylinder
case class Cone(minimum: Double = Double.NegativeInfinity,
                maximum: Double = Double.PositiveInfinity,
                closed: Boolean = false,
                transform: Matrix4x4 = Matrix4x4.Identity,
                material: Material = Material(),
                var parent: Shape = null) extends Shape {

  override def localIntersect(ray: Ray): Intersections = {
    val a = ray.direction.x * ray.direction.x - ray.direction.y * ray.direction.y + ray.direction.z * ray.direction.z
    val b = 2 * ray.origin.x * ray.direction.x - 2 * ray.origin.y * ray.direction.y + 2 * ray.origin.z * ray.direction.z
    val c = ray.origin.x * ray.origin.x - ray.origin.y * ray.origin.y + ray.origin.z * ray.origin.z

    if (approximatelyEqual(a, 0.0) && !approximatelyEqual(b, 0.0)) {
      val t = -c / (2 * b)
      val y0 = ray.origin.y + (t * ray.direction.y)
      if (minimum < y0 && y0 < maximum) {
        return Intersections(intersectCaps(ray) ::: Intersection(t, this) :: Nil) //todo refactor
      }
    }

    val discriminant = b * b - 4 * a * c
    if (discriminant < 0) {
      return Intersections(intersectCaps(ray))
    }

    //TODO implement new math object for solving this
    val t0 = (-b - math.sqrt(discriminant)) / (2 * a)
    val t1 = (-b + math.sqrt(discriminant)) / (2 * a)

    val xsCylinder = List(t0, t1)
      .sorted
      .map(t => (t, ray.origin.y + t * ray.direction.y))
      .filter(t2y => minimum < t2y._2 && t2y._2 < maximum)
      .map(t2y => Intersection(t2y._1, this))

    val xsCaps = intersectCaps(ray)

    Intersections(xsCaps ::: xsCylinder)
  }

  private def intersectCaps(ray: Ray): List[Intersection] = {
    var result: List[Intersection] = Nil //todo refactor

    if (!closed || approximatelyEqual(ray.direction.y, 0.0)) {
      return result
    }

    val t0 = (minimum - ray.origin.y) / ray.direction.y
    if (checkCap(ray, t0, minimum)) {
      result ::= Intersection(t0, this)
    }

    val t1 = (maximum - ray.origin.y) / ray.direction.y
    if (checkCap(ray, t1, maximum)) {
      result ::= Intersection(t1, this)
    }

    result
  }

  private def checkCap(ray: Ray, t: Double, radius: Double): Boolean = {
    val x = ray.origin.x + t * ray.direction.x
    val z = ray.origin.z + t * ray.direction.z
    (x * x + z * z) <= radius * radius
  }

  override def localNormalAt(point: Tuple, intersection: Intersection): Tuple = {
    val dist = point.x * point.x + point.z * point.z

    if (dist < 1 && (point.y >= maximum - EPSILON)) { //todo remove reference to epsilon
      Vector(0, 1, 0)
    } else if (dist < 1 && (point.y <= minimum + EPSILON)) {
      Vector(0, -1, 0)
    } else {
      val y0 = math.hypot(point.x, point.z)
      val y = if (point.y > 0) -y0 else y0
      Vector(point.x, y, point.z)
    }
  }
}

//todo implement as immutable, with builder
case class Group(children: ListBuffer[Shape] = ListBuffer(),
                 transform: Matrix4x4 = Matrix4x4.Identity,
                 material: Material = Material(),
                 var parent: Shape = null) extends Shape {

  override def localIntersect(ray: Ray): Intersections =
    children.foldLeft(Intersections(Nil))((acc, s) => acc ::: s.intersect(ray))


  override def localNormalAt(point: Tuple, intersection: Intersection): Tuple = ???

  def add(shape: Shape): Unit = {
    children += shape
    shape.parent = this //todo refactor
  }

  def contains(shape: Shape): Boolean = children.contains(shape)

  def size: Int = children.length

  override def toString: String = s"children: ${children.length}, parent: $parent" //todo fix me
}


case class Triangle(p1: Tuple,
                    p2: Tuple,
                    p3: Tuple,
                    transform: Matrix4x4 = Matrix4x4.Identity,
                    material: Material = Material(),
                    var parent: Shape = null) extends Shape {

  val e1: Tuple = p2 - p1
  val e2: Tuple = p3 - p1
  val normal: Tuple = e2.cross(e1).normalize

  override def localIntersect(ray: Ray): Intersections = {
    val dirCrossE2 = ray.direction cross e2
    val det = e1 dot dirCrossE2
    if (approximatelyEqual(det, 0.0)) {
      return Intersections(Nil)
    }

    val f = 1.0 / det
    val p1ToOrigin = ray.origin - p1
    val u = f * p1ToOrigin.dot(dirCrossE2)
    if (u < 0 || 1 < u) {
      return Intersections(Nil)
    }

    val originCrossE1 = p1ToOrigin cross e1
    val v = f * ray.direction.dot(originCrossE1)
    if (v < 0 || 1 < (u + v)) {
      return Intersections(Nil)
    }

    val t = f * e2.dot(originCrossE1)
    Intersections(Intersection(t, this) :: Nil)
  }

  override def localNormalAt(point: Tuple, intersection: Intersection): Tuple = normal

}

case class SmoothTriangle(
                           p1: Tuple,
                           p2: Tuple,
                           p3: Tuple,
                           n1: Tuple,
                           n2: Tuple,
                           n3: Tuple,
                           transform: Matrix4x4 = Matrix4x4.Identity,
                           material: Material = Material(),
                           var parent: Shape = null) extends Shape {

  val e1: Tuple = p2 - p1
  val e2: Tuple = p3 - p1
  val normal: Tuple = e2.cross(e1).normalize

  override def localIntersect(ray: Ray): Intersections = {
    val dirCrossE2 = ray.direction cross e2
    val det = e1 dot dirCrossE2
    if (approximatelyEqual(det, 0.0)) {
      return Intersections(Nil)
    }

    val f = 1.0 / det
    val p1ToOrigin = ray.origin - p1
    val u = f * p1ToOrigin.dot(dirCrossE2)
    if (u < 0 || 1 < u) {
      return Intersections(Nil)
    }

    val originCrossE1 = p1ToOrigin cross e1
    val v = f * ray.direction.dot(originCrossE1)
    if (v < 0 || 1 < (u + v)) {
      return Intersections(Nil)
    }

    val t = f * e2.dot(originCrossE1)
    Intersections(Intersection(t, this, u, v) :: Nil)
  }

  override def localNormalAt(point: Tuple, hit: Intersection): Tuple =
    n2 * hit.u + n3 * hit.v + n1 * (1 - hit.u - hit.v)

}

object Operation extends Enumeration {
  type Operation = Value
  val union, intersection, difference = Value

  //replace with case classes?
  def intersectionAllowed(op: Operation, lhit: Boolean, inl: Boolean, inr: Boolean): Boolean =
    op match {
      case `union` => (lhit && !inr) || (!lhit && !inl)
      case `intersection` => (lhit && inr) || (!lhit && inl)
      case `difference` => (lhit && !inr) || (!lhit && inl)
    }
}

case class Csg(operation: Operation,
               left: Shape,
               right: Shape,
               transform: Matrix4x4 = Matrix4x4.Identity,
               material: Material = Material(),
               var parent: Shape = null) extends Shape {

  left.parent = this
  right.parent = this

  //todo refactor, replace with polymorphism
  private def includes(a: Shape, b: Shape): Boolean =
    a match {
      case csg: Csg => List(csg.left, csg.right).exists(includes(_, b))
      case g: Group => g.children.exists(includes(_, b))
      case _ => a == b
    }

  override def localIntersect(ray: Ray): Intersections =
    filterIntersections(left.intersect(ray) ::: right.intersect(ray))

  def filterIntersections(intersections: Intersections): Intersections = {
    var inl = false
    var inr = false

    val result = intersections
      .filter(i => {
        val lhit = includes(left, i.obj)
        val allowed = Operation.intersectionAllowed(operation, lhit, inl, inr)
        if (lhit) {
          inl = !inl
        } else {
          inr = !inr
        }
        allowed
      })
    Intersections(result.toList)
  }

  override def localNormalAt(point: Tuple, hit: Intersection): Tuple = ???

  override def toString: String = s"CSG($operation, left, right)"
}

