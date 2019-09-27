package ray.tracer

trait Precision[T] {

  def precision: T

  def approximatelyEqual(x: T, y: T): Boolean

  def approximatelyLess(x: T, y: T): Boolean

  def approximatelyGreater(x: T, y: T): Boolean

}


object Precision {

  implicit object PrecisionDouble extends Precision[Double] {

    override val precision: Double = 0.00001

    override def approximatelyEqual(x: Double, y: Double): Boolean = math.abs(x - y) < precision

    override def approximatelyLess(x: Double, y: Double): Boolean = x <= y + precision

    override def approximatelyGreater(x: Double, y: Double): Boolean = x >= y - precision

  }

}