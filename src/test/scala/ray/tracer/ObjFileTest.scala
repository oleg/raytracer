package ray.tracer

import org.scalatest.FunSuite

import scala.io.Source

class ObjFileTest extends FunSuite {

  test("Ignoring unrecognized lines") {
    val gibberish =
      """There was a young lady named Bright​
        |who traveled much faster than light.​
        ​|She set out one day​
        ​|in a relative way,​
        |and came back the previous night.​""".stripMargin

    val objFile = new ObjFileParser().parse(Source.fromString(gibberish))
    assert(objFile.ignored == 5)
  }

  test("Vertex records") {
    val file =
      """
        |v -1 1 0
        |v -1.0000 0.5000 0.0000
        |v 1 0 0
        |v 1 1 0
      """.stripMargin

    val objFile = new ObjFileParser().parse(Source.fromString(file))
    assert(objFile.vertices(1) == Point(-1, 1, 0))
    assert(objFile.vertices(2) == Point(-1, 0.5, 0))
    assert(objFile.vertices(3) == Point(1, 0, 0))
    assert(objFile.vertices(4) == Point(1, 1, 0))
  }

  test("Parsing triangle faces") {
    val file =
      """
        |v -1 1 0
        |v -1 0 0
        |v 1 0 0
        |v 1 1 0
        |
        |f 1 2 3
        |f 1 3 4
      """.stripMargin

    val objFile = new ObjFileParser().parse(Source.fromString(file))
    val g: Group = objFile.groups("default")
    val t1 = g.children(0).asInstanceOf[Triangle]
    val t2 = g.children(1).asInstanceOf[Triangle]

    assert(t1.p1 == objFile.vertices(1))
    assert(t1.p2 == objFile.vertices(2))
    assert(t1.p3 == objFile.vertices(3))

    assert(t2.p1 == objFile.vertices(1))
    assert(t2.p2 == objFile.vertices(3))
    assert(t2.p3 == objFile.vertices(4))
  }

  test("Triangulating polygons") {
    val file =
      """
        |v -1 1 0
        |v -1 0 0
        |v 1 0 0
        |v 1 1 0
        |v 0 2 0
        |
        |f 1 2 3 4 5
      """.stripMargin

    val objFile = new ObjFileParser().parse(Source.fromString(file))
    val g: Group = objFile.groups("default")
    val t1 = g.children(0).asInstanceOf[Triangle]
    val t2 = g.children(1).asInstanceOf[Triangle]
    val t3 = g.children(2).asInstanceOf[Triangle]

    assert(t1.p1 == objFile.vertices(1))
    assert(t1.p2 == objFile.vertices(2))
    assert(t1.p3 == objFile.vertices(3))

    assert(t2.p1 == objFile.vertices(1))
    assert(t2.p2 == objFile.vertices(3))
    assert(t2.p3 == objFile.vertices(4))

    assert(t3.p1 == objFile.vertices(1))
    assert(t3.p2 == objFile.vertices(4))
    assert(t3.p3 == objFile.vertices(5))
  }

  test("Triangles in groups") {
    val objFile = new ObjFileParser().parse(Source.fromResource("triangles.obj"))
    val g1 = objFile.groups("FirstGroup")
    val g2 = objFile.groups("SecondGroup")
    val t1 = g1.children.head.asInstanceOf[Triangle]
    val t2 = g2.children.head.asInstanceOf[Triangle]

    assert(t1.p1 == objFile.vertices(1))
    assert(t1.p2 == objFile.vertices(2))
    assert(t1.p3 == objFile.vertices(3))

    assert(t2.p1 == objFile.vertices(1))
    assert(t2.p2 == objFile.vertices(3))
    assert(t2.p3 == objFile.vertices(4))
  }

  test("Converting an OBJ file to a group") {
    val objFile = new ObjFileParser().parse(Source.fromResource("triangles.obj"))
    val g: Group = objFile.mainGroup
    val g1 = objFile.groups("FirstGroup")
    val g2 = objFile.groups("SecondGroup")

    assert(g.children.contains(g1))
    assert(g.children.contains(g2))
  }
}
