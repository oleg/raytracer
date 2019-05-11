package ray.tracer

//todo add validation for min 0, max 1
case class Material(color: Color = Color(1, 1, 1),
                    ambient: Double = 0.1,
                    diffuse: Double = 0.9,
                    specular: Double = 0.9,
                    shininess: Double = 200.0,
                    reflective: Double = 0.0,
                    transparency: Double = 0.0,
                    refractiveIndex: Double = 1.0,
                    pattern: Pattern = null) {

  def lighting(light: PointLight,
               shape: Shape,
               point: Tuple,
               eyev: Tuple,
               normalv: Tuple,
               inShadow: Boolean): Color = {
    val material = this

    val color = Option(pattern).map(_.patternAtShape(shape, point)).getOrElse(material.color)
    val efectiveColor = color * light.intensity
    val lightv = (light.position - point).normalize
    val ambient = efectiveColor * material.ambient

    val lightDotNormal = lightv dot normalv
    if (lightDotNormal < 0 || inShadow) {
      return ambient
    }

    val diffuse = efectiveColor * material.diffuse * lightDotNormal
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
