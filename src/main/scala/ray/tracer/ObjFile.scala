package ray.tracer

import scala.collection.mutable
import scala.io.Source

//case?
class ObjFile(val mainGroup: Group,
              val groups: Map[String, Group],
              val vertices: Map[Int, Tuple],
              val ignored: Int) {

}

class ObjFileParser {

  var ignored: Int = 0
  var vertexIndex: Int = 0

  val vertices: mutable.Map[Int, Tuple] = mutable.HashMap.empty //Int, Point

  var currentGroup: Group = Group()
  val groups: mutable.Map[String, Group] = mutable.HashMap.empty

  groups.put("default", currentGroup)

  def nextVertexIndex(): Int = {
    vertexIndex += 1
    vertexIndex
  }

  def parse(source: Source): ObjFile = {
    source
      .getLines
      .foreach(line => {
        if (line.startsWith("v ")) {
          consumeSafe(line, parseVertex)
        } else if (line.startsWith("g")) {
          consumeSafe(line, parseGroupName)
        } else if (line.startsWith("f")) {
          consumeSafe(line, parseFace)
        } else {
          ignored += 1
        }
      })

    val mainGroup = Group()
    groups.values.foreach(mainGroup.add(_))

    new ObjFile(mainGroup, groups.toMap, vertices.toMap, ignored)
  }

  //todo refactor
  private def parseVertex(line: String): Unit = {
    val items = line.split("\\s+")
    val point = Point(items(1).toDouble, items(2).toDouble, items(3).toDouble)
    vertices.put(nextVertexIndex(), point)
  }

  //todo refactor
  private def parseFace(line: String): Unit = {
    val items = line.split("\\s+")
    val a = vertices(parseItem(items(1)))
    items.drop(2)
      .map(parseItem)
      .map(vertices(_))
      .sliding(2)
      .map(pair => Triangle(a, pair(0), pair(1)))
      .foreach(currentGroup.add(_))
  }

  private def parseItem(item: String): Int = {
    val i = item.indexOf("/")
    (if (i != -1) {
      item.substring(0, i)
    } else {
      item
    }).toInt
  }

  //todo refactor
  private def parseGroupName(line: String): Unit = {
    val items = line.split("\\s+")
    currentGroup = Group()
    groups.put(items(1), currentGroup)
  }

  private def consumeSafe[T](line: String, f: String => T): Unit =
    try {
      f(line)
    } catch {
      case e: RuntimeException => throw e //e.printStackTrace()
    }
}

