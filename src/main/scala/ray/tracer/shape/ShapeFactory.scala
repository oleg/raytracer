package ray.tracer.shape

import ray.tracer.shapemath._
import ray.tracer.{Material, Matrix4x4, Point, Vector, shape}

object ShapeFactory {

  def glassSphere(transform: Matrix4x4 = Matrix4x4.Identity,
                  material: Material = Material(transparency = 1.0, refractiveIndex = 1.5)): Shape =
    Sphere(transform, material)

  def Sphere(transform: Matrix4x4 = Matrix4x4.Identity,
             material: Material = Material(),
             parent: Shape = null): Shape =
    SimpleShape(SphereMath(), transform, material, parent)

  def Plane(transform: Matrix4x4 = Matrix4x4.Identity,
            material: Material = Material(),
            parent: Shape = null): Shape =
    shape.SimpleShape(PlaneMath(), transform, material, parent)

  def Cube(transform: Matrix4x4 = Matrix4x4.Identity,
           material: Material = Material(),
           parent: Shape = null): Shape =
    shape.SimpleShape(CubeMath(), transform, material, parent)

  def Cylinder(minimum: Double = Double.NegativeInfinity,
               maximum: Double = Double.PositiveInfinity,
               closed: Boolean = false,
               transform: Matrix4x4 = Matrix4x4.Identity,
               material: Material = Material(),
               parent: Shape = null): Shape =
    shape.SimpleShape(CylinderMath(minimum, maximum, closed), transform, material, parent)

  def Cone(minimum: Double = Double.NegativeInfinity,
           maximum: Double = Double.PositiveInfinity,
           closed: Boolean = false,
           transform: Matrix4x4 = Matrix4x4.Identity,
           material: Material = Material(),
           parent: Shape = null): Shape =
    shape.SimpleShape(ConeMath(minimum, maximum, closed), transform, material, parent)

  def Triangle(p1: Point,
               p2: Point,
               p3: Point,
               transform: Matrix4x4 = Matrix4x4.Identity,
               material: Material = Material(),
               parent: Shape = null): Shape =
    shape.SimpleShape(TriangleMath(p1, p2, p3), transform, material, parent)

  def SmoothTriangle(p1: Point,
                     p2: Point,
                     p3: Point,
                     n1: Vector,
                     n2: Vector,
                     n3: Vector,
                     transform: Matrix4x4 = Matrix4x4.Identity,
                     material: Material = Material(),
                     parent: Shape = null): Shape =
    shape.SimpleShape(SmoothTriangleMath(p1, p2, p3, n1, n2, n3), transform, material, parent)

}
