package ray.tracer

import ray.shapes._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

case class ObjFile(
                    mainGroup: Group,
                    groups: Map[String, Group],
                    vertices: List[Point],
                    normals: List[Vector],
                    ignored: Int)

class ObjFileParser {

  private val vertices: ArrayBuffer[Point] = ArrayBuffer()
  vertices += null

  private val normals: ArrayBuffer[Vector] = ArrayBuffer()
  normals += null //todo hack fix me

  private val mainGroup: Group = Group()
  private val groups: mutable.Map[String, Group] = mutable.HashMap.empty
  private var currentGroup: Group = _
  private var ignored: Int = 0

  parseGroupName("g default")

  case class Item(vertexIndex: Int, normalIndex: Int)

  def parse(source: Source): ObjFile = {
    source
      .getLines
      .foreach(line => {
        if (line.startsWith("vn ")) {
          parseNormal(line)
        } else if (line.startsWith("v ")) {
          parseVertex(line)
        } else if (line.startsWith("g")) {
          parseGroupName(line)
        } else if (line.startsWith("f")) {
          parseFace(line)
        } else {
          ignored += 1
        }
      })

    ObjFile(mainGroup, groups.toMap, vertices.toList, normals.toList, ignored)
  }

  //todo refactor
  private def parseFace(line: String): Unit = {
    val items = line.split("\\s+")
    val item = parseItem(items(1))
    val smooth = item.normalIndex > 0 && item.normalIndex < normals.length

    items.drop(2)
      .map(parseItem)
      .sliding(2)
      .map(pair => {
        if (smooth) {
          smoothTriangle(item, pair(0), pair(1))
        } else {
          triangle(item, pair(0), pair(1))
        }
      })
      .foreach(currentGroup.add(_))
  }

  //todo refactor
  private def parseVertex(line: String): Unit = {
    val items = line.split("\\s+")
    val point = Point(items(1).toDouble, items(2).toDouble, items(3).toDouble)
    vertices += point
  }

  private def parseNormal(line: String): Unit = {
    val items = line.split("\\s+")
    val vector = Vector(items(1).toDouble, items(2).toDouble, items(3).toDouble)
    normals += vector
  }

  private def parseItem(item: String): Item = {
    val parts = item.split("/")
    if (parts.length == 3) {
      Item(parts(0).toInt, parts(2).toInt)
    } else {
      Item(parts(0).toInt, -1)
    }
  }

  //todo refactor
  private def parseGroupName(line: String): Unit = {
    val items = line.split("\\s+")
    currentGroup = Group()
    mainGroup.add(currentGroup) //todo refactor
    groups.put(items(1), currentGroup)
  }

  private def triangle(i1: Item, i2: Item, i3: Item): Triangle = {
    Triangle(
      vertices(i1.vertexIndex),
      vertices(i2.vertexIndex),
      vertices(i3.vertexIndex))
  }

  //todo refactor
  private def smoothTriangle(i1: Item, i2: Item, i3: Item): SmoothTriangle = {
    SmoothTriangle(
      vertices(i1.vertexIndex),
      vertices(i2.vertexIndex),
      vertices(i3.vertexIndex),
      normals(i1.normalIndex),
      normals(i2.normalIndex),
      normals(i3.normalIndex))
  }
}

