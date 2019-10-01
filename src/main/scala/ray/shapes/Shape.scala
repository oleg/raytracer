package ray.shapes

import ray.tracer.{Intersection, Intersections, Material, Matrix4x4, Point, Ray, Vector}

import scala.annotation.tailrec

trait Shape {

  val transform: Matrix4x4
  val material: Material

  //oh-oh-oh
  val parent: Shape

  parentSet(parent)


  //this method is invoked on children & parent classes
  //parent classes forward every intersection to child localIntersect
  def intersect(worldRay: Ray): Intersections = {
    val localRay = worldRay.transform(transform.inverse)
    localIntersect(localRay)
  }

  protected def localIntersect(ray: Ray): Intersections


  //only invoked on actual shapes, that's why they have to hold link to parent
  def normalAt(worldPoint: Point, intersection: Intersection): Vector = {
    val objectPoint = worldToObject(worldPoint)
    val normal = localNormalAt(objectPoint, intersection)
    normalToWorld(normal)
  }

  //invoked from Intersection on actual shapes
  def worldToObject(point: Point): Point =
    transform.inverse * (if (parent != null) parent.worldToObject(point) else point)

  //only invoked on actual shapes, that's why they have to hold link to parent
  protected def localNormalAt(point: Point, intersection: Intersection): Vector

  @tailrec
  private def normalToWorld(normal: Vector): Vector = {
    val n = (transform.inverse.transpose * normal).normalize
    if (parent != null) parent.normalToWorld(n) else n
  }

  private def parentSet(shape: Shape): Unit = {
    if (parent != null)
      parent.newChildrenAdded(this)
  }

  protected def newChildrenAdded(shape: Shape): Unit = {
    //override if needed
  }

}
