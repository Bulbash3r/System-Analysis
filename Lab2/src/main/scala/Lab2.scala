import breeze.linalg.DenseVector
import breeze.stats._

import scala.io.{BufferedSource, Source}

object Lab2 {

  /**
    * @return надёжность оценки
    */
  def getReliability: Double = {
    0.95
  }

  /**
    * @return имя dataset - файла
    */
  def getFilename: String = {
    "dataset.txt"
  }

  /**
    * Метод парсинга датасета
    * @return mpg - DenseVector с данными
    */
  def parseFile(filename: String): (DenseVector[Double], DenseVector[Double]) = {

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

  def main (args: Array[String]): Unit = {
    val (mpg, horsepower) = parseFile(getFilename)  //данные

    /*дисперсия*/
    val mpgDespersion        = variance(mpg)
    val horsepowerDespersion = variance(horsepower)
    /*мат.ожидание*/
    val mpgAverage           = mean(mpg)
    val horsepowerAverage    = mean(horsepower)


  }
}
