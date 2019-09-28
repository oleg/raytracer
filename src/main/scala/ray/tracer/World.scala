package ray.tracer
import ray.shapes._
import ray.shapes.Sphere

case class World(light: PointLight, shapes: List[Shape]) {

  def contains(sphere: Sphere): Boolean = shapes.contains(sphere)

  def colorAt(r: Ray, leftIterations: Int = 5): Color = {
    val intersections = intersect(r)
    intersections
      .hit
      .map(i => shadeHit(i.prepareComputations(r, intersections), leftIterations))
      .getOrElse(Color.black)
  }

  def shadeHit(comps: Computation, leftIterations: Int = 5): Color = {
    val shadowed = isShadowed(comps.overPoint)

    val surface = comps.obj.material.lighting(
      light, comps.obj, comps.overPoint, comps.eyev, comps.normalv, shadowed)

    val reflected = reflectedColor(comps, leftIterations)
    val refracted = refractedColor(comps, leftIterations)

    val m = comps.obj.material
    if (m.reflective > 0 && m.transparency > 0) {
      val reflectance = comps.schlick()
      return surface + reflected * reflectance + refracted * (1 - reflectance)
    }
    surface + reflected + refracted
  }

  def reflectedColor(comps: Computation, leftIterations: Int = 5): Color = {
    if (leftIterations < 1) {
      return Color.black
    }
    if (comps.obj.material.reflective == 0) {
      return Color.black
    }

    val reflectRay = Ray(comps.overPoint, comps.reflectv)
    colorAt(reflectRay, leftIterations - 1) * comps.obj.material.reflective
  }

  def refractedColor(comps: Computation, leftIterations: Int = 5): Color = {
    if (leftIterations < 1) {
      return Color.black
    }
    if (comps.obj.material.transparency == 0) {
      return Color.black
    }
    val nRatio = comps.n1 / comps.n2
    val cosI = comps.eyev dot comps.normalv
    val sin2t = math.pow(nRatio, 2) * (1 - math.pow(cosI, 2))
    if (sin2t > 1) {
      return Color.black
    }

    val cosT = math.sqrt(1.0 - sin2t)
    val refractv = comps.normalv * (nRatio * cosI - cosT) - comps.eyev * nRatio

    val refractRay = Ray(comps.underPoint, refractv)
    colorAt(refractRay, leftIterations - 1) * comps.obj.material.transparency
  }

  def isShadowed(point: Point): Boolean = {
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