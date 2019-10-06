package ray.tracer

import ray.tracer.Operation.Operation
import ray.tracer.shapemath.{CubeMath, Inter, PlaneMath, SphereMath}

import scala.collection.mutable.ListBuffer


trait Shape {

  val transform: Matrix4x4

  val material: Material

  var parent: Shape //todo should not return Group

  def localIntersect(ray: Ray): Intersections

  def localNormalAt(point: Point, intersection: Intersection): Vector

  def intersect(worldRay: Ray): Intersections = {
    val ray = worldRay.transform(transform.inverse)
    localIntersect(ray)
  }

  def normalAt(worldPoint: Point, intersection: Intersection): Vector = {
    val localPoint: Point = worldToObject(worldPoint)
    val localNormal: Vector = localNormalAt(localPoint, intersection)
    normalToWorld(localNormal)
  }

  def worldToObject(point: Point): Point = {
    transform.inverse * (if (parent != null) parent.worldToObject(point) else point)
  }

  def normalToWorld(normal: Vector): Vector = {
    val n = (transform.inverse.transpose * normal).normalize
    if (parent != null) parent.normalToWorld(n) else n
  }

}

case class Sphere(transform: Matrix4x4 = Matrix4x4.Identity,
                  material: Material = Material(),
                  var parent: Shape = null) extends Shape {

  private val sphereMath = SphereMath()

  override def localIntersect(ray: Ray): Intersections =
    Intersections(sphereMath.intersect(ray).map(i => Intersection(i.t, this, i.u, i.v)))

  override def localNormalAt(point: Point, intersection: Intersection): Vector =
    sphereMath.normalAt(point, Option(intersection).map(i => Inter(i.t, i.u, i.v)).orNull)

}

object Sphere {

  def glass(transform: Matrix4x4 = Matrix4x4.Identity,
            material: Material = Material(transparency = 1.0, refractiveIndex = 1.5)): Sphere =
    Sphere(transform, material)

}

case class Plane(transform: Matrix4x4 = Matrix4x4.Identity,
                 material: Material = Material(),
                 var parent: Shape = null) extends Shape {

  private val planeMath = PlaneMath()

  override def localIntersect(ray: Ray): Intersections =
    Intersections(planeMath.intersect(ray).map(i => Intersection(i.t, this, i.u, i.v)))

  override def localNormalAt(point: Point, intersection: Intersection): Vector =
    planeMath.normalAt(point, Option(intersection).map(i => Inter(i.t, i.u, i.v)).orNull)

}

case class Cube(transform: Matrix4x4 = Matrix4x4.Identity,
                material: Material = Material(),
                var parent: Shape = null) extends Shape {

  private val cubeMath = CubeMath()

  override def localIntersect(ray: Ray): Intersections =
    Intersections(cubeMath.intersect(ray).map(i => Intersection(i.t, this, i.u, i.v)))

  override def localNormalAt(point: Point, intersection: Intersection): Vector =
    cubeMath.normalAt(point, Option(intersection).map(i => Inter(i.t, i.u, i.v)).orNull)

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

    if (implicitly[Precision[Double]].approximatelyEqual(a, 0.0)) {
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

    if (!closed || implicitly[Precision[Double]].approximatelyEqual(ray.direction.y, 0.0)) {
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

  override def localNormalAt(point: Point, intersection: Intersection): Vector = {
    val dist = point.x * point.x + point.z * point.z
    val p = implicitly[Precision[Double]] //todo fix me
    if (dist < 1 && p.approximatelyGreater(point.y, maximum)) {
      Vector(0, 1, 0)
    } else if (dist < 1 && p.approximatelyLess(point.y, minimum)) {
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

    if (implicitly[Precision[Double]].approximatelyEqual(a, 0.0) && !implicitly[Precision[Double]].approximatelyEqual(b, 0.0)) {
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

    if (!closed || implicitly[Precision[Double]].approximatelyEqual(ray.direction.y, 0.0)) {
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

  override def localNormalAt(point: Point, intersection: Intersection): Vector = {
    val dist = point.x * point.x + point.z * point.z
    //p.approximatelyLess(point.y, minimum)
    val p = implicitly[Precision[Double]]
    if (dist < 1 && p.approximatelyGreater(point.y, maximum)) { //todo remove reference to epsilon
      Vector(0, 1, 0)
    } else if (dist < 1 && p.approximatelyLess(point.y, minimum)) {
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


  override def localNormalAt(point: Point, intersection: Intersection): Vector = ???

  def add(shape: Shape): Unit = {
    children += shape
    shape.parent = this //todo refactor
  }

  def contains(shape: Shape): Boolean = children.contains(shape)

  def size: Int = children.length

  override def toString: String = s"children: ${children.length}, parent: $parent" //todo fix me
}


case class Triangle(p1: Point,
                    p2: Point,
                    p3: Point,
                    transform: Matrix4x4 = Matrix4x4.Identity,
                    material: Material = Material(),
                    var parent: Shape = null) extends Shape {

  val e1: Vector = p2 - p1
  val e2: Vector = p3 - p1
  val normal: Vector = e2.cross(e1).normalize

  override def localIntersect(ray: Ray): Intersections = {
    val dirCrossE2 = ray.direction cross e2
    val det = e1 dot dirCrossE2
    if (implicitly[Precision[Double]].approximatelyEqual(det, 0.0)) {
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

  override def localNormalAt(point: Point, intersection: Intersection): Vector = normal

}

case class SmoothTriangle(
                           p1: Point,
                           p2: Point,
                           p3: Point,
                           n1: Vector,
                           n2: Vector,
                           n3: Vector,
                           transform: Matrix4x4 = Matrix4x4.Identity,
                           material: Material = Material(),
                           var parent: Shape = null) extends Shape {

  val e1: Vector = p2 - p1
  val e2: Vector = p3 - p1
  val normal: Vector = e2.cross(e1).normalize

  override def localIntersect(ray: Ray): Intersections = {
    val dirCrossE2 = ray.direction cross e2
    val det = e1 dot dirCrossE2
    if (implicitly[Precision[Double]].approximatelyEqual(det, 0.0)) {
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

  override def localNormalAt(point: Point, hit: Intersection): Vector = {
    val vector0 = n2 * hit.u
    val vector1 = n3 * hit.v
    val vector2 = n1 * (1 - hit.u - hit.v)
    (vector0 + vector1 + vector2)
  }

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

  override def localNormalAt(point: Point, hit: Intersection): Vector = ???

  override def toString: String = s"CSG($operation, left, right)"
}

