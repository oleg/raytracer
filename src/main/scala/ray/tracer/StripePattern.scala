package ray.tracer

case class StripePattern(a: Color, b: Color, transform: Matrix4x4 = Matrix4x4.Identity) {

  def stripeAtObject(shape: Shape, point: Tuple): Color = {
    val objPoint = shape.transform.inverse * point
    val patternPoint = this.transform.inverse * objPoint
    stripeAt(patternPoint)
  }

  def stripeAt(point: Tuple): Color = {
    val useA = math.floor(point.x) % 2 == 0
    if (useA) a else b
  }

}
