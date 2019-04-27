package ray

case class Computation(t: Double,
                       obj: Sphere,
                       point: Tuple,
                       eyev: Tuple,
                       normalv: Tuple,
                       inside: Boolean) {

}

case class Intersection(t: Double,
                        obj: Sphere) {

  def prepareComputations(ray: Ray): Computation = {

    val point = ray.position(t)
    val eyev = -ray.direction
    val normalv = obj.normalAt(point)
    val inside = (normalv dot eyev) < 0

    Computation(t, obj, point, eyev, if (inside) -normalv else normalv, inside)
  }

}


case class Intersections private(is: List[Intersection]) {

  def apply(i: Int): Intersection = is(i)

  def length: Int = is.length

  def hit: Option[Intersection] = is.find(_.t >= 0)

}

object Intersections {
  private val vectorOrdering = Ordering.by((_: Intersection).t)

  def apply(is: List[Intersection]): Intersections = new Intersections(is.sorted(vectorOrdering))
}