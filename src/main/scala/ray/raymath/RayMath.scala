package ray.raymath

object RayMath {

  val Sqrt2: Double = math.sqrt(2)

  val Sqrt2Div2: Double = math.sqrt(2) / 2.0

  def solveQuadratic(a: Double, b: Double, c: Double): Option[(Double, Double)] = {
    val discriminant = b * b - 4 * a * c
    if (discriminant < 0) {
      return None
    }

    val sd = math.sqrt(discriminant)
    val t1 = (-b - sd) / (2 * a)
    val t2 = (-b + sd) / (2 * a)
    Some((t1, t2))
  }

}