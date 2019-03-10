import java.text.DecimalFormat

import scala.io.{BufferedSource, Source}
import breeze.linalg._
import breeze.stats._
import breeze.numerics._
import breeze.plot._
import breeze.stats.regression._
import org.jfree.chart.annotations.XYTextAnnotation

object Lab1 {

  /**
    * @return name of dataset file
    */
  def getFilename: String = {
    "dataset.txt"
  }

  /**
    * Parse dataset file
    * @return (mpg, horsepower) - DenseVectors with data
    */
  def parseFile(filename: String,
               ): (DenseVector[Double], DenseVector[Double]) = {

    val mpg = DenseVector.zeros[Double](392)
    val horsepower = DenseVector.zeros[Double](392)

    val file: BufferedSource = Source.fromFile(filename)
    val lines = file.getLines().toList
    var i = 0

    for (line <- lines) {
      val temp = line.split(" ")
      if (temp(3) != "?") {
        mpg(i) = temp(0).toDouble
        horsepower(i) = temp(3).toDouble
        i+=1
      }
    }
    (mpg, horsepower)
  }

  /**
    * @return correlation coefficient
    */
  def getCorrelation(mpg: DenseVector[Double],
                     horsepower: DenseVector[Double]): Double = {

    val averageMpg = mean(mpg)
    val dispersionMpg = variance(mpg)

    val averageHorsepower = mean(horsepower)
    val dispersionHorsepower = variance(horsepower)

    val standardDeviationMpg = sqrt(dispersionMpg)
    val standardDeviationHorsepower = sqrt(dispersionHorsepower)

    1.0 / (mpg.length - 1.0) * sum (((mpg - averageMpg) / standardDeviationMpg) :* ((horsepower - averageHorsepower) / standardDeviationHorsepower))
  }

  /**
    * Print plots of linear regression and negative curvilinear dependence
    */
  def printGraph(mpg: DenseVector[Double],
                 horsepower: DenseVector[Double],
                 k: Double,
                 b: Double): Unit = {
    val f = Figure("Linear regression")
    val p = f.subplot(0)
    val x = linspace (13.2, 48)
    val x1 = linspace (min(mpg), max(mpg))
    val equation = (k :* x1) :+ b

    val annotation1 = f"Linear regression: y = " + new DecimalFormat("#0.00").format(k) + " * x + " + new DecimalFormat("#0.00").format(b)
    val annotation2 = f"Optimal plot: y = 500 / (x - 10.5) + 49"
    p.plot.addAnnotation(new XYTextAnnotation(annotation1, 39, 217.5))
    p.plot.addAnnotation(new XYTextAnnotation(annotation2, 39, 212.5))

    p += plot(mpg, horsepower, '.', colorcode = "black", name = "Dots")
    p += plot(x1, equation, name = "Linear regression")
    p += plot(x, 500.0 / (x - 10.5) + 49.0, name = "Optimal plot")    //отрицательная криволинейная зависимость

    p.xlabel = "mpg"
    p.ylabel = "horsepower"
    p.title  = "Linear regression"
    p.legend = true
    f.height = 900
    f.width  = 1500
    f.saveas("lines.png")
  }

  /**
    * @return coefficients of linear regression equalition
    */
  def getLinearRegression(mpg: DenseVector[Double], horsepower: DenseVector[Double]): (Double, Double) = {
    val features = DenseMatrix.horzcat(
      DenseMatrix.ones[Double](mpg.length, 1),
      mpg.toDenseMatrix.t
    )
    val leastSquaresResult = leastSquares(features, horsepower)
    (leastSquaresResult.coefficients(1), leastSquaresResult.coefficients(0))
  }

  def main (args: Array[String]): Unit = {
    val (mpg, horsepower) = parseFile (getFilename)
    val (k, b) = getLinearRegression(mpg, horsepower)
    printGraph(mpg, horsepower, k, b)
    println("Correlation coefficient: " + getCorrelation(mpg, horsepower))
    println("Linear regression: y = " + new DecimalFormat("#0.000000").format(k) + " * x + " + new DecimalFormat("#0.000000").format(b))
    println("Optimal plot: y = 500 / (x - 10.5) + 49")
  }
}