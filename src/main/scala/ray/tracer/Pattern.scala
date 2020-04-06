package ray.tracer

import ray.tracer.shape.Shape

trait Pattern {

  val transform: Matrix4x4

  def patternAtShape(objPoint: Point): Color = {
    patternAt(this.transform.inverse * objPoint)
  }

  def patternAt(point: Point): Color

}

case class StripePattern(a: Color,
                         b: Color,
                         transform: Matrix4x4 = Matrix4x4.Identity) extends Pattern {

  def patternAt(point: Point): Color = {
    val useA = point.x.floor % 2 == 0
    if (useA) a else b
  }

}

case class ColorPattern(transform: Matrix4x4 = Matrix4x4.Identity) extends Pattern {

  def patternAt(point: Point): Color = Color(point.x, point.y, point.z)

}


case class GradientPattern(a: Color,
                           b: Color,
                           transform: Matrix4x4 = Matrix4x4.Identity) extends Pattern {

  override def patternAt(point: Point): Color = {
    val distance = b - a
    val fraction = point.x - point.x.floor
    a + distance * fraction
  }

}

case class RingPattern(a: Color,
                       b: Color,
                       transform: Matrix4x4 = Matrix4x4.Identity) extends Pattern {

  override def patternAt(point: Point): Color = {
    val useA = math.hypot(point.x, point.z).floor % 2 == 0
    if (useA) a else b
  }

}

case class CheckersPattern(a: Color,
                           b: Color,
                           transform: Matrix4x4 = Matrix4x4.Identity) extends Pattern {

  override def patternAt(point: Point): Color = {
    val useA = (point.x.floor + point.y.floor + point.z.floor) % 2 == 0
    if (useA) a else b
  }

}


