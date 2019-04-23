package ray


object Intersection {

  def apply(t: Double, obj: Sphere): Intersection = new Intersection(t, obj)

}

class Intersection(val t: Double, val obj: Sphere) {

}


object Intersections {

  def apply(is: Intersection*): Intersections = new Intersections(is: _ *)

}

class Intersections(val is: Intersection*) {

  private val ord = Ordering.by((_: Intersection).t)

  def apply(i: Int): Intersection = is(i)

  def count: Int = is.length

  def hit: Option[Intersection] = is.filter(_.t >= 0).reduceOption(ord.min)

}