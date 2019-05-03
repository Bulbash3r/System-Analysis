import breeze.linalg._
import breeze.stats._
import breeze.numerics._

import scala.io.{BufferedSource, Source}

object main {

  /**
    * Критическое значение (граница области значения критерия,
    * для которых гипотеха отвергается. Находится по таблице
    * значений функции Лапласа
    * Ф(х) = 0.95/2 = 0.475
    * х = 1.96
    */
  def getT: Double = {
    1.96
  }

  def getArraySize: Int = {
    392
  }

  def getFilename: String = {
    "dataset.txt"
  }

  def checkKnown(mpg: DenseVector[Double],
                 horsepower: DenseVector[Double]): Boolean = {
    val tExp = (mean(mpg) - mean(horsepower)) / Math.sqrt((variance(mpg) / mpg.length) + (variance(horsepower) / horsepower.length))
    tExp < getT
  }

  /**
    * Parse dataset file
    * @return (mpg, horsepower) - DenseVectors with data
    */
  def parseFile(filename: String,
               ): (DenseVector[Double], DenseVector[Double]) = {

    val mpg = DenseVector.zeros[Double](getArraySize)
    val horsepower = DenseVector.zeros[Double](getArraySize)

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

  def getMeanInterval (array: DenseVector[Double]): Array[Double] = {
    val e = (getT * sqrt(variance(array))) / Math.sqrt(array.length)
    val meanArray = mean(array)
    Array(meanArray - e, meanArray + e)
  }

  def getDispersionInterval (array: DenseVector[Double]): Array[Double] = {
    var hi1 = 439.1645955
    var hi2 = 347.1088338
    Array((array.length - 1) * variance(array)/hi1, (array.length - 1) * variance(array)/hi2)
  }

  def checkUnknown(mpg: DenseVector[Double],
                   horsepower: DenseVector[Double]): Boolean = {
    val left = (mean(mpg) - mean(horsepower)) / ((mpg.length - 1) * variance(mpg) + (horsepower.length - 1) * variance(horsepower))
    val right = Math.sqrt(mpg.length * horsepower.length * (mpg.length + horsepower.length - 2) / (mpg.length + horsepower.length))
    Math.abs(left*right) < getT
  }

  def main(args: Array[String]): Unit = {
    val (mpg, horsepower) = parseFile(getFilename)

    val meanMpgInterval = getMeanInterval(mpg)
    print("Доверительный интервал мат.ожидания первой величины: ")
    println (meanMpgInterval(0), meanMpgInterval(1))

    val meanHorsepowerInterval = getMeanInterval(horsepower)
    print("Доверительный интервал мат.ожидания второй величины: ")
    println (meanHorsepowerInterval(0), meanHorsepowerInterval(1))

    val dispersionMpgInterval = getDispersionInterval(mpg)
    print("Доверительный интервал дисперсии первой величины: ")
    println (dispersionMpgInterval(0), dispersionMpgInterval(1))

    val dispersionHorsepowerInterval = getDispersionInterval(horsepower)
    print("Доверительный интервал дисперсии второй величины: ")
    println (dispersionHorsepowerInterval(0), dispersionHorsepowerInterval(1))

    println("Проверка гипотезы о равенстве мат.ожиданий:")
    println("При известной дисперсии: " + checkKnown(mpg, horsepower))
    println("При неизвестной дисперсии: " + checkUnknown(mpg, horsepower))
  }
}