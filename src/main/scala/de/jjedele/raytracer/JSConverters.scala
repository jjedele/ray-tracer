package de.jjedele.raytracer

import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, html}

object JSConverters {

  /**
   * Convert canvas to HTML5 canvas.
   *
   * @param canvas
   * @return
   */
  def toHTMLCanvas(canvas: Canvas): dom.html.Canvas = {
    val wCanvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
    wCanvas.height = canvas.height
    wCanvas.width = canvas.width

    val ctx = wCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    val imgData = ctx.getImageData(0, 0, wCanvas.width, wCanvas.height)

    // TODO: is there a faster way of doing this?
    val flatPixels = canvas.data.flatMap { row =>
      row.array.flatMap { col =>
        (col * 255).data.map(x => Math.min(255, Math.max(0, x))).map(_.toInt) ++
          Array(255) // alpha channel
      }
    }
    (0 until imgData.data.size).foreach { idx =>
      imgData.data(idx) = flatPixels(idx)
    }

    ctx.putImageData(imgData, 0, 0)

    wCanvas
  }

}
