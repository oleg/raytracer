package ray.tracer

case class Computation(t: Double,
                       obj: Shape,
                       point: Tuple,
                       overPoint: Tuple,
                       eyev: Tuple,
                       normalv: Tuple,
                       inside: Boolean) {

}

case class Intersection(t: Double,
                        obj: Shape) {

  def prepareComputations(ray: Ray): Computation = {

    val point = ray.position(t)
    val eyev = -ray.direction
    val normalv = obj.normalAt(point)
    val inside = (normalv dot eyev) < 0
    val overPoint = point + normalv * EPSILON

    Computation(t, obj, point, overPoint, eyev, if (inside) -normalv else normalv, inside)
  }

}


case class Intersections private(private val is: List[Intersection]) {

  def apply(i: Int): Intersection = is(i)

  def length: Int = is.length

  def hit: Option[Intersection] = is.find(_.t >= 0)

  def :::(other: Intersections): Intersections = Intersections(is ::: other.is)

}

object Intersections {
  private val vectorOrdering = Ordering.by((_: Intersection).t)

  def apply(is: List[Intersection]): Intersections = new Intersections(is.sorted(vectorOrdering))
}