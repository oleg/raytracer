package ray.tracer

/**
  * The world object described here supports only a single light source,
  * but it’s not terribly difficult to support more than one.
  * You would need to make sure your shade_hit function iterates over all of the light sources,
  * calling lighting for each one and adding the colors together.
  */
case class World(light: PointLight, shapes: List[Shape]) {

  def contains(sphere: Sphere): Boolean = shapes.contains(sphere)

  def colorAt(r: Ray, leftIterations: Int = 5): Color =
    intersect(r)
      .hit
      .map(i => shadeHit(i.prepareComputations(r), leftIterations))
      .getOrElse(Color.black)

  def shadeHit(comps: Computation, leftIterations: Int = 5): Color = {
    val shadowed = isShadowed(comps.overPoint)
    val surface = comps.obj.material.lighting(light, comps.obj, comps.overPoint, comps.eyev, comps.normalv, shadowed)
    val reflected = reflectedColor(comps, leftIterations)
    surface + reflected
  }

  def reflectedColor(comps: Computation, leftIterations: Int = 5): Color = {
    if (leftIterations < 1) {
      return Color.black
    }
    if (comps.obj.material.reflective == 0) {
      return Color.black
    }

    val reflectRay = Ray(comps.overPoint, comps.reflectv)
    val color = colorAt(reflectRay, leftIterations - 1)

    color * comps.obj.material.reflective
  }

  def isShadowed(point: Tuple): Boolean = {
    val v = light.position - point
    val distance = v.magnitude
    val direction = v.normalize

    val ray = Ray(point, direction)

    intersect(ray)
      .hit
      .exists(_.t < distance)
  }

  def intersect(r: Ray): Intersections = shapes.foldLeft(Intersections(Nil))((acc, s) => acc ::: s.intersect(r))

}