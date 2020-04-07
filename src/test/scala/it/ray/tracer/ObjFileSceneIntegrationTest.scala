package it.ray.tracer

import org.scalatest.FunSuite
import ray.tracer._
import ray.tracer.shape.Group

import scala.io.Source
import scala.math.Pi

class ObjFileSceneIntegrationTest extends FunSuite {

  //todo takes too long ≈30 seconds
  ignore("generate scene from object file") {

    val obj = new ObjFileParser().parse(Source.fromResource("teapot-low.obj"))
    //todo fix
    val group1 = Group(
      transform = Matrix4x4.Identity
        .rotateX(-Pi / 2)
        .translate(0, -5, 0))

    group1.add(obj.mainGroup)

    val world = World(
      PointLight(Point(20, 20, 30), Color.white),
      group1 :: Nil
    )
    val camera = Camera(500, 250, Pi / 2, Matrix4x4.viewTransform(Point(0, 10, 20), Point(0, 1, 0), Vector(0, 1, 0)))
    val canvas = camera.renderConcurrently(world)

    //    canvas.dumpPpmTo("object-scene-500x250.ppm")
    assert(canvas.toPpm == testutil.Sources.readString("/it/object-scene-500x250.ppm"))
  }

}
