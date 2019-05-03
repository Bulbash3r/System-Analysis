import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import breeze.linalg._
//import breeze.stats._
import breeze.numerics._
import breeze.plot._
import javax.imageio.ImageIO

object main {

  def rgbToGray (color: Int): Int = {
    val red = (color & 0xff0000) / 65536
    val green = (color & 0xff00) / 256
    val blue = color & 0xff
    (red + green + blue) / 3
  }

  def getBargraph(vector: DenseVector[Int]): DenseVector[Int] = {
    val bargraph = DenseVector.zeros[Int](256)
    vector.foreach(bargraph(_) += 1)
    bargraph
  }

  def makeGray(img: BufferedImage): (BufferedImage, DenseVector[Int]) = {
    val colordata = DenseVector.zeros[Int](img.getHeight * img.getWidth)
    for {
      w <- (0 until img.getWidth).toVector
      h <- (0 until img.getHeight).toVector
    } yield {
      val color = img.getRGB(w, h)
      val graycolor = rgbToGray(color)
      colordata(w * img.getWidth + h) = graycolor
      img.setRGB(w, h, new Color(graycolor, graycolor, graycolor).getRGB)
    }
    (img, colordata)
  }

  def printGraph(gist: DenseVector[Int]): Unit = {
    val f = Figure("Linear regression")
    val p = f.subplot(0)
    val x = linspace (min(gist), max(gist))

    p += hist(gist, 256)

    f.height = 900
    f.width  = 1500
    f.saveas("lines.png")
  }

  def main(args: Array[String]): Unit = {
    val firstImg = ImageIO.read(new File("pic1.jpg"))
    val secondImg = ImageIO.read(new File("pic3.jpg"))

    val (firstImgGray, firstImgData) = makeGray(firstImg)
    val (secondImgGray, secondImgData) = makeGray(secondImg)

    ImageIO.write(firstImgGray, "jpg", new File("graypic1.jpg"))
    ImageIO.write(secondImgGray, "jpg", new File("graypic2.jpg"))

    val secondImgBargraph = getBargraph(secondImgData)
    println(secondImgBargraph)
    printGraph(secondImgData)
  }
}
