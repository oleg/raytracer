package ray

import org.scalatest.FunSuite

class ColorTest extends FunSuite {

  test("Colors are (red, green, blue) tuples") {
    val c = Color(-0.5, 0.4, 1.7)

    assert(c.red == -0.5)
    assert(c.green == 0.4)
    assert(c.blue == 1.7)
  }

  test("Adding colors") {
    val c1 = Color(0.9, 0.6, 0.75)
    val c2 = Color(0.7, 0.1, 0.25)

    assert(c1 + c2 == Color(1.6, 0.7, 1.0))
  }

  test("Subtracting colors") {
    val c1 = Color(0.9, 0.6, 0.75)
    val c2 = Color(0.7, 0.1, 0.25)

    assert(c1 - c2 == Color(0.20000000000000007, 0.5, 0.5)) //meh, doubles
  }

  test("Multiplying a color by a scalar") {
    val c = Color(0.2, 0.3, 0.4)

    assert(c * 2 == Color(0.4, 0.6, 0.8))
  }

  test("Multiplying colors") {
    val c1 = Color(1, 0.2, 0.4)
    val c2 = Color(0.9, 1, 0.1)

    assert(c1 * c2 == Color(0.9, 0.2, 0.04000000000000001)) //meh, doubles, do I want to do something about that?
  }

}
