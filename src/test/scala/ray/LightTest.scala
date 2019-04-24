package ray

import org.scalatest.FunSuite

class LightTest extends FunSuite {

  test("A point light has a position and intensity") {
    val intensity = Color(1, 1, 1)
    val position = Point(0, 0, 0)
    val light = PointLight(position, intensity)
    assert(light.position == position)
    assert(light.intensity == intensity)
  }

}
