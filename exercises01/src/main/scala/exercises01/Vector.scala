package exercises01

import scala.util.Try

class Vector(val x: Double, val y: Double) {
  def +(other: Vector): Vector = new Vector(x + other.x, y + other.y)

  def -(other: Vector): Vector = new Vector(x - other.x, y - other.y)

  def *(scalar: Double): Vector = new Vector(x * scalar, y * scalar)

  def unary_- : Vector = this * (-1)

  def euclideanLength: Double = math.sqrt(x * x + y * y)

  def normalized: Vector = {
    val euclidLength = euclideanLength
    Try(this * (1 / euclidLength)).getOrElse(this)
  }

  override def equals(other: Any): Boolean = other match {
    case that: Vector => x == that.x && y == that.y
    case _ => false
  }
  // Vector(x, y)
  override def toString: String = s"Vector($x, $y)"
}

object Vector {
  def fromAngle(angle: Double, length: Double): Vector = new Vector(math.cos(angle) * length, math.sin(angle) * length)

  def sum(list: List[Vector]): Vector = list.foldLeft(new Vector(0, 0))(_ + _)

  def unapply(arg: Vector): Option[(Double, Double)] = Some((arg.x, arg.y))
}
