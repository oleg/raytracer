package ray.tracer.shape

import ray.tracer.shapemath._
import ray.tracer.{Material, Matrix4x4, Point, Vector, shape}

object ShapeFactory {

  def glassSphere(transform: Matrix4x4 = Matrix4x4.Identity,
                  material: Material = Material(transparency = 1.0, refractiveIndex = 1.5)): Shape =
    Sphere(transform, material)

  def Sphere(transform: Matrix4x4 = Matrix4x4.Identity,
             material: Material = Material()): Shape =
    SimpleShape(SphereMath(), transform, material)

  def Plane(transform: Matrix4x4 = Matrix4x4.Identity,
            material: Material = Material()): Shape =
    shape.SimpleShape(PlaneMath(), transform, material)

  def Cube(transform: Matrix4x4 = Matrix4x4.Identity,
           material: Material = Material()): Shape =
    shape.SimpleShape(CubeMath(), transform, material)

  def Cylinder(minimum: Double = Double.NegativeInfinity,
               maximum: Double = Double.PositiveInfinity,
               closed: Boolean = false,
               transform: Matrix4x4 = Matrix4x4.Identity,
               material: Material = Material()): Shape =
    shape.SimpleShape(CylinderMath(minimum, maximum, closed), transform, material)

  def Cone(minimum: Double = Double.NegativeInfinity,
           maximum: Double = Double.PositiveInfinity,
           closed: Boolean = false,
           transform: Matrix4x4 = Matrix4x4.Identity,
           material: Material = Material()): Shape =
    shape.SimpleShape(ConeMath(minimum, maximum, closed), transform, material)

  def Triangle(p1: Point,
               p2: Point,
               p3: Point,
               transform: Matrix4x4 = Matrix4x4.Identity,
               material: Material = Material()): Shape =
    shape.SimpleShape(TriangleMath(p1, p2, p3), transform, material)

  def SmoothTriangle(p1: Point,
                     p2: Point,
                     p3: Point,
                     n1: Vector,
                     n2: Vector,
                     n3: Vector,
                     transform: Matrix4x4 = Matrix4x4.Identity,
                     material: Material = Material()): Shape =
    shape.SimpleShape(SmoothTriangleMath(p1, p2, p3, n1, n2, n3), transform, material)

}
