package ray.tracer

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

case class Camera(hszie: Int, vsize: Int, fieldOfView: Double, transform: Matrix4x4 = Matrix4x4.Identity) {

  private val halfView: Double = math.tan(fieldOfView / 2)

  private val aspect: Double = hszie / vsize.asInstanceOf[Double]

  private val (halfWidth: Double, halfHeight: Double) =
    if (aspect >= 1) (halfView, halfView / aspect)
    else (halfView * aspect, halfView)

  val pixelSize: Double = (halfWidth * 2) / hszie.asInstanceOf[Double]

  def rayForPixel(px: Int, py: Int): Ray = {
    val xOffset = (px + 0.5) * pixelSize
    val yOffset = (py + 0.5) * pixelSize

    val worldX = halfWidth - xOffset
    val worldY = halfHeight - yOffset

    val pixel = transform.inverse * Point(worldX, worldY, -1)
    val origin = transform.inverse * Point(0, 0, 0)
    val direction = (pixel - origin).normalize

    Ray(origin, direction)
  }

  def render(world: World): Canvas = {
    val canvas = Canvas(hszie, vsize)
    for (y <- 0 until vsize) {
      for (x <- 0 until hszie) {
        val ray = rayForPixel(x, y)
        val color = world.colorAt(ray)
        canvas(x, y) = color
      }
    }
    canvas
  }

  def renderConcurrently(world: World): Canvas = {
    val canvas = Canvas(hszie, vsize)
    val batchSize = vsize / Runtime.getRuntime.availableProcessors
    import scala.concurrent.ExecutionContext.Implicits.global
    val steps: Iterator[List[Int]] = 0.to(vsize, batchSize).toList.sliding(2)
    val tasks = for (List(start, end) <- steps) yield Future {
      for (y <- start until end) {
        for (x <- 0 until hszie) {
          val ray = rayForPixel(x, y)
          canvas(x, y) = world.colorAt(ray)
        }
      }
    }
    val aggregate = Future.sequence(tasks)
    Await.result(aggregate, Duration(1, TimeUnit.DAYS))
    canvas
  }
}
