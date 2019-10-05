package testutil

import scala.io.Source

object Sources {

  def readString(filename: String): String = {
    Source.fromURL(getClass.getResource(filename)).mkString
  }

}
