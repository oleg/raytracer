package ray.tracer

case class Sphere(transform: Matrix4x4 = Matrix4x4.Identity, material: Material = Material()) {

  //todo how to make accept only points?

  def normalAt(worldPoint: Tuple): Tuple = {
    val objectPoint = transform.inverse * worldPoint
    val objectNormal = objectPoint - Point(0, 0, 0)
    val worldNormal = transform.inverse.transpose * objectNormal
    worldNormal.toVector.normalize
  }

}
