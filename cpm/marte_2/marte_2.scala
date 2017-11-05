import math._
import scala.util._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 */
object Player {
  def loadMars: (Int, Int, Int, Int) = {
    var x1, y1, x2, y2, lastX, lastY = -1
    val n = readInt // the number of points used to draw the surface of Mars.
    for (i <- 0 until n) {
      // land_x: X coordinate of a surface point. (0 to 6999)
      // land_y: Y coordinate of a surface point. By linking all the points together in a sequential fashion, you form the surface of Mars.
      val Array(x, y) = for (i <- readLine split " ") yield i.toInt
      if (y == lastY) {
        x1 = lastX
        y1 = lastY
        x2 = x
        y2 = y
      }
      lastX = x
      lastY = y
    }
    if (x1 > x2) {
      lastX = x1
      lastY = y1
      x1 = x2
      y1 = y2
      x2 = lastX
      y2 = lastY
      
    }
    (x1, y1, x2, y2)
  }

  def evalDirection(pos: String, x: Int, y: Int, hs: Int, vs: Int, r: Int, p: Int, tx: Int, ty: Int): (Int, Int) = {
    Console.err.println("POS " + pos)
    var angle = r
    var power = p
    pos match {
      case "SX" => {
        if (hs <= 20) {
          angle = rotate(r, -15)

        } else {
          angle = rotate(r, +15, -90, 45)

        }
        power = accellerate(p)
      }
      case "DX" => {
        var momento = ((abs(vs) > 40, abs(hs) > 20)) match {
          case (true, true) => (-45, +1)
          case (true, false) => (45, +1)
          case (false, true) => (-90, +1)
          case _ => (45, -1)
          
        }
        
        Console.err.println(momento)
        
        angle = momento._1 match {
          case 0 => rotate(r, -r, true)
          case _ => rotate(r, momento._1 - r)
        }
        
        power = momento._2 match {
          case 0 => p
          case 1 => accellerate(p)
          case -1 => decellerate(p)
        }
//        if (hs < 0 && abs((x - tx) / hs) < abs(hs / 4) ) {
//          if (r > 0) {
//        	  angle = rotate(r, -15, true)
//          } else if (r < 0) {
//              angle = rotate(r, +15, true)
//          }
//          
//        } else if (hs >= -20) {
//          Console.err.println("A")
//          angle = rotate(r, +15)
//        } else {
//          Console.err.println("B")
//          angle = rotate(r, -15, -45, 90)
//        }
        power = accellerate(p)
      }
      case "ON" =>
        {
          if (r == 0 && hs == 0) {
            if (vs <= -40) {
              power = accellerate(p)
            } else if (vs > -40) {
              power = decellerate(p)
            }
          } else if (hs > 0 && r < 0) {
            power = decellerate(p)
            angle = rotate(r, +15, -90, 0)
          } else if (hs < 0 && r > 0) {
            power = decellerate(p)
            angle = rotate(r, -15, 0, +90)
          } else if (hs > 20 || hs < -20) {
            power = accellerate(p)
          } else {
            if (vs <= -40) {
              power = accellerate(p)
            } else if (vs > -40) {
              power = decellerate(p)
            }
            angle = 0
          }
        }
    }

    (angle, power)
  }

  def main(args: Array[String]) {
    val plan = loadMars
    val x1 = plan._1
    val y1 = plan._2
    val x2 = plan._3
    val y2 = plan._4

    while (true) {
      // hs: the horizontal speed (in m/s), can be negative.
      // vs: the vertical speed (in m/s), can be negative.
      // f: the quantity of remaining fuel in liters.
      // r: the rotation angle in degrees (-90 to 90).
      // p: the thrust power (0 to 4).
      val Array(x, y, hs, vs, f, r, p) = for (i <- readLine split " ") yield i.toInt

      val direction = (signum(x - x1), signum(x - x2)) match {
        case (-1, -1) => evalDirection("SX", x, y, hs, vs, r, p, x1, y1)
        case (1, 1) => evalDirection("DX", x, y, hs, vs, r, p, x2, y2)
        case _ => evalDirection("ON", x, y, hs, vs, r, p, x1, y1)
      }

      println(direction._1 + " " + direction._2) // R P. R is the desired rotation angle. P is the desired thrust power.
    }
  }

  def rotate(actual: Int, rotation: Int): Int = {
    rotate(actual, rotation, -90, 90)
  }

  def rotate(actual: Int, rotation: Int, keep: Boolean): Int = {
    var angle = actual + rotation
    if (angle < -90) {
      angle = -90
    } else if (angle > 90) {
      angle = 90
    }
    if (keep && signum(actual) != signum(angle)) {
      angle = 0
    }
    angle
  }

  def rotate(actual: Int, rotation: Int, min: Int, max: Int): Int = {
    var angle = actual + rotation
    if (angle < min) {
      angle = min
    } else if (angle > max) {
      angle = max
    }
    angle
  }

  def accellerate(actual: Int): Int = {
    var power = actual + 1
    if (power > 4) {
      power = 4
    }
    power
  }

  def decellerate(actual: Int): Int = {
    var power = actual - 1
    if (power < 0) {
      power = 0
    }
    power
  }
}
