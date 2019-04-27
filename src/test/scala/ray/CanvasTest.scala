package ray

import org.scalatest.FunSuite

class CanvasTest extends FunSuite {

  test("Creating a canvas") {
    val c = Canvas(10, 20)

    assert(c.width == 10)
    assert(c.height == 20)

    for {
      i <- 0 until c.width
      j <- 0 until c.height
    } assert(c(i, j) == Color(0.0, 0.0, 0.0))
  }

  test("Writing pixels to a canvas") {
    val c = Canvas(10, 20)
    val red = Color(1, 0, 0)

    c(2, 3) = red

    assert(c(2, 3) == red)
  }

  //todo move to separate class?
  test("Constructing the PPM header") {
    val c = Canvas(5, 3)

    val ppm = c.toPpm

    val lines1to3 = ppm.split("\n").take(3).mkString("\n")
    assert(lines1to3 ==
      """
        |P3
        |5 3
        |255
        |""".stripMargin.trim)
  }

  test("Constructing the PPM pixel data") {
    val c = Canvas(5, 3)
    val c1 = Color(1.5, 0, 0)
    val c2 = Color(0, 0.5, 0)
    val c3 = Color(-0.5, 0, 1)

    c(0, 0) = c1
    c(2, 1) = c2
    c(4, 2) = c3

    val ppm = c.toPpm

    val lines4to6 = ppm.split("\n").slice(3, 6).mkString("\n")
    assert(lines4to6 ==
      """
        |255 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        |0 0 0 0 0 0 0 128 0 0 0 0 0 0 0
        |0 0 0 0 0 0 0 0 0 0 0 0 0 0 255
        |""".stripMargin.trim)
  }

  test("Splitting long lines in PPM files") {
    val c = Canvas(10, 2, Color(1, 0.8, 0.6))

    val ppm = c.toPpm

    val lines4to7 = ppm.split("\n").slice(3, 7).mkString("\n")
    assert(lines4to7 ==
      """
        |255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204
        |153 255 204 153 255 204 153 255 204 153 255 204 153
        |255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204
        |153 255 204 153 255 204 153 255 204 153 255 204 153
        |""".stripMargin.trim)
  }

  test("PPM files are terminated by a newline character") {
    val c = Canvas(5, 3)

    val ppm = c.toPpm

    assert(ppm.last == '\n')
  }
}
