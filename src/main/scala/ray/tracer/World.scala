package ray.tracer

/**
  * The world object described here supports only a single light source,
  * but it’s not terribly difficult to support more than one.
  * You would need to make sure your shade_hit function iterates over all of the light sources,
  * calling lighting for each one and adding the colors together.
  */
case class World(light: PointLight, spheres: List[Sphere]) {

  def contains(sphere: Sphere): Boolean = spheres.contains(sphere)

  def shadeHit(comps: Computation): Color =
    comps.obj.material.lighting(light, comps.overPoint, comps.eyev, comps.normalv, isShadowed(comps.overPoint))

  def colorAt(r: Ray): Color =
    r.intersect(this)
      .hit
      .map(i => shadeHit(i.prepareComputations(r)))
      .getOrElse(Color(0, 0, 0))

  def isShadowed(point: Tuple): Boolean = {
    val v = light.position - point
    val distance = v.magnitude
    val direction = v.normalize

    Ray(point, direction)
      .intersect(this)
      .hit
      .exists(_.t < distance)
  }

}