import math._
import scala.util._

object Solution extends App {
    def inRing(x: Int, y: Int, w: Int, h: Int, ring: Int): Boolean = {
        //Console.err.println(s"($i,$j) h:$h w:$w ring:$ring")
        (x >= ring &&  x < (w-ring)) && (y >= ring && y < (h-ring))
    }
    
    val Array(w, h) = for(i <- readLine split " ") yield i.toInt
    val d = readInt
    val source = Array.ofDim[Array[Int]](h)
    val target = Array.ofDim[Array[Int]](h)
    for(i <- 0 until h) {
        source(i) = for (j <- readLine split " ") yield j.toInt
        target(i) = Array.ofDim[Int](w)
    }

    for(i <- 0 until h) {
        Console.err.println(source(i).mkString(" ")) 
    }

    val halfRing = min(w,h)/2
    Console.err.println(s"d:$d w:$w h:$h hr:$halfRing")
    
    
    for(c <- 0 until w) {
        for (r <- 0 until h) {
            val (nr, nc) = {
                val ring = min(
                    min(c, w - c - 1),
                    min(r, h - r -1 )
                )
                if (ring < halfRing) {
                    val cupper = w - 1 - ring
                    val rupper = h - 1 - ring
                    var (dr,dc, rl) = (r,c) match {
                        case (a, _) if a == ring => (r, c-d, 1)
                        case (a, _) if a == rupper => (r, c+d, 2)
                        case (_, b) if b == ring => (r+d, c, 3)
                        case (_, _) => (r-d, c, 4)
                    }
                    //val v = source(r)(c)
                    //Console.err.println(s"($r,$c)->($dr,$dc) [$v] ring:$ring $rupper $cupper $rl")
                    while (!inRing(dc, dr, w, h, ring)) {
                        if (dc < ring) {
                            dr = ring + ring - dc 
                            dc = ring
                        }
                        if (dr > rupper) {
                            dc = dr - rupper + ring
                            dr = rupper
                        }
                        if (dc > cupper) {
                            dr = rupper - (dc - cupper) 
                            dc = cupper
                        }
                        if (dr < ring) {
                            dc = cupper + dr - ring
                            dr = ring 
                        }
                    }
                    //Console.err.println(s"($r,$c)->($dr,$dc) [$v] ring:$ring")
                    (dr, dc)
                } else {
                    (r, c)
                }
            }
            target(nr)(nc)=source(r)(c)
        }
    }
    
    for(i <- 0 until h) {
        println(target(i).mkString(" ")) 
    }    
}
