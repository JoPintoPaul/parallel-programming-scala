
import common._

import scalashop._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))

    def apply(x: Int, y: Int): RGBA = data(y * width + x)

    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    var pixels: List[RGBA] = List()

    var currentX = x - radius
    while (currentX <= x + radius) {
      var currentY = y - radius
      while (currentY <= y + radius) {
        pixels = pixels :+ src(
          clamp(currentX, 0, src.width - 1),
          clamp(currentY, 0, src.height - 1)
        )
        currentY = currentY + 1
      }
      currentX = currentX + 1
    }

    val channels = pixels.distinct.map(p => (red(p), green(p), blue(p), alpha(p)))
    rgba(
      channels.map(_._1).sum / channels.length,
      channels.map(_._2).sum / channels.length,
      channels.map(_._3).sum / channels.length,
      channels.map(_._4).sum / channels.length
    )
  }


  def breakpoints(dimension: Int, numTasks: Int): List[(Int, Int)] = {
    val partitions: Int = Math.max(dimension / numTasks, 1)
    val range: Range = 0 to dimension by partitions
    val topIndex = if (numTasks > range.length) range.length else numTasks

    val incompleteBreakpoints: List[(Int, Int)] = (for (i <- 1 until topIndex) yield {
      (range(i - 1), range(i))
    }).toList

    calculateRange(dimension, numTasks)

    incompleteBreakpoints.length match {
      case 0 => List((0, dimension))
      case i => incompleteBreakpoints.last._2 match {
        case add if add < dimension => incompleteBreakpoints :+(add, dimension)
        case _ => incompleteBreakpoints
      }
    }
  }

  def calculateRange(dimension: Int, numTasks: Int) = {
    val step: Int = Math.max(dimension / numTasks, 1)
    println(s"partitions: $step")
    val range: Range = 0 to dimension by step
    val topIndex = Math.min(numTasks, range.length)
      //if (numTasks > range.length) range.length else numTasks

    val incompleteBreakpoints: List[(Int, Int)] = (for (i <- 1 until topIndex) yield {
      (range(i - 1), range(i))
    }).toList

    val breaks = incompleteBreakpoints.length match {
      case 0 => List((0, dimension))
      case i => incompleteBreakpoints.last._2 match {
        case add if add < dimension => incompleteBreakpoints :+(add, dimension)
        case _ => incompleteBreakpoints
      }
    }

    println(s"incomplete breaks: $incompleteBreakpoints")
    println(s"old style breaks: $breaks")

    incompleteBreakpoints
  }
}
