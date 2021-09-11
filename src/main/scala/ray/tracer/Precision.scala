package ray.tracer

trait Precision[T]:
  def precision: T

  def approximatelyEqual(x: T, y: T): Boolean

  def approximatelyLess(x: T, y: T): Boolean

  def approximatelyGreater(x: T, y: T): Boolean

given PrecisionDouble: Precision[Double] with
  def precision: Double = 0.00001

  def approximatelyEqual(x: Double, y: Double): Boolean =
    math.abs(x - y) < precision

  def approximatelyLess(x: Double, y: Double): Boolean =
    x <= y + precision

  def approximatelyGreater(x: Double, y: Double): Boolean =
    x >= y - precision
//given PrecisionDouble: Precision[Double] = Precision[Double]()

//object Precision {
//
//  implicit val PrecisionDouble: Precision[Double] = CustomPrecisionDouble(0.00001)
//
//  case class CustomPrecisionDouble(precision: Double) extends Precision[Double] {
//
//    override def approximatelyEqual(x: Double, y: Double): Boolean =
//      math.abs(x - y) < precision
//
//    override def approximatelyLess(x: Double, y: Double): Boolean =
//      x <= y + precision
//
//    override def approximatelyGreater(x: Double, y: Double): Boolean =
//      x >= y - precision
//
//  }
//
//
//  //  implicit object PrecisionDouble extends Precision[Double] {
//  //
//  //    override val precision: Double = 0.00001
//  //
//  //    override def approximatelyEqual(x: Double, y: Double): Boolean = math.abs(x - y) < precision
//  //
//  //    override def approximatelyLess(x: Double, y: Double): Boolean = x <= y + precision
//  //
//  //    override def approximatelyGreater(x: Double, y: Double): Boolean = x >= y - precision
//  //
//  //  }
//
//}