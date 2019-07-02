package ray.tracer

//todo implement : Nested, Blended and Perturbed patterns
trait Pattern {

  val transform: Matrix4x4

  def patternAtShape(shape: Shape, point: Tuple): Color = {
    val objPoint = shape.worldToObject(point)
    val patternPoint = this.transform.inverse * objPoint
    patternAt(patternPoint)
  }

  def patternAt(point: Tuple): Color

}

case class StripePattern(a: Color,
                         b: Color,
                         transform: Matrix4x4 = Matrix4x4.Identity) extends Pattern {

  def patternAt(point: Tuple): Color = {
    val useA = point.x.floor % 2 == 0
    if (useA) a else b
  }

}

case class ColorPattern(transform: Matrix4x4 = Matrix4x4.Identity) extends Pattern {

  def patternAt(point: Tuple): Color = Color(point.x, point.y, point.z)

}


case class GradientPattern(a: Color,
                           b: Color,
                           transform: Matrix4x4 = Matrix4x4.Identity) extends Pattern {

  override def patternAt(point: Tuple): Color = {
    val distance = b - a
    val fraction = point.x - point.x.floor
    a + distance * fraction
  }

}

case class RingPattern(a: Color,
                       b: Color,
                       transform: Matrix4x4 = Matrix4x4.Identity) extends Pattern {

  override def patternAt(point: Tuple): Color = {
    val useA = math.hypot(point.x, point.z).floor % 2 == 0
    if (useA) a else b
  }

}

case class CheckersPattern(a: Color,
                           b: Color,
                           transform: Matrix4x4 = Matrix4x4.Identity) extends Pattern {

  override def patternAt(point: Tuple): Color = {
    val useA = (point.x.floor + point.y.floor + point.z.floor) % 2 == 0
    if (useA) a else b
  }

}


