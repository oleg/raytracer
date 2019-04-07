package ray

// A projectile has a position (a point) and a velocity (a vector).
// An environment has gravity (a vector) and wind (a vector).

class Projectile(val position: Point, val velocity: Vector) {

  def tick(env: Environment): Projectile = {
    val pos: Tuple = position + velocity
    val vel: Tuple = velocity + env.gravity + env.wind
    return new Projectile(pos, vel)
  }

}

class Environment(val gravity: Vector, val wind: Vector)


object Asdf {

  //  ​ 	​function​ tick(env, proj)
  //  ​ 	  position ← proj.position + proj.velocity
  //  ​ 	  velocity ← proj.velocity + env.gravity + env.wind
  //  ​ 	  ​return​ projectile(position, velocity)
  //  ​ 	​end​ ​function​


}