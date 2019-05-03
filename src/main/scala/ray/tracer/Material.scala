package ray.tracer

//todo add validation for min 0, max 1
case class Material(color: Color = Color(1, 1, 1),
                    ambient: Double = 0.1,
                    diffuse: Double = 0.9,
                    specular: Double = 0.9,
                    shininess: Double = 200.0,
                    pattern: StripePattern = null) {

  def lighting(light: PointLight, shape: Shape, point: Tuple, eyev: Tuple, normalv: Tuple, inShadow: Boolean): Color = {
    val material = this

    val color = Option(pattern).map(_.stripeAtObject(shape, point)).getOrElse(material.color)

    // combine the surface color with the light's color/intensity​
    val efectiveColor = color * light.intensity

    // find the direction to the light source​
    val lightv = (light.position - point).normalize

    // compute the ambient contribution​
    val ambient = efectiveColor * material.ambient

    // light_dot_normal represents the cosine of the angle between the​
    // light vector and the normal vector. A negative number means the​
    // light is on the other side of the surface.​
    val lightDotNormal = lightv dot normalv

    if (lightDotNormal < 0 || inShadow) {
      return ambient
    }
    // compute the diffuse contribution​
    val diffuse = efectiveColor * material.diffuse * lightDotNormal

    // reflect_dot_eye represents the cosine of the angle between the​
    // reflection vector and the eye vector. A negative number means the​
    // light reflects away from the eye.​
    val reflectv = (-lightv).reflect(normalv)
    val reflectDotEye = reflectv dot eyev

    if (reflectDotEye <= 0) {
      return ambient + diffuse
    }

    val factor = math.pow(reflectDotEye, material.shininess)
    val specular = light.intensity * material.specular * factor

    return ambient + diffuse + specular
  }

}
