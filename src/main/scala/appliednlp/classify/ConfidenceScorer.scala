package appliednlp.classify

import scala.io.Source

/**
 * An application that takes a gold labeled file and a file containing
 * predictions, and then computes the accuracy for the top-third most
 * confident instances and the accuracy for the bottom-third (least 
 * confident instances).
 */
object ConfidenceScorer {

  def main(args: Array[String]) {
    val Array(testFile, predictionsFile) = args

    val actual = Source.fromFile(testFile).getLines.flatMap { line =>
      line.split(",").takeRight(1)
    }.toList

    val probabilityAndPrediction = Source.fromFile(predictionsFile).getLines.map { line =>
      line.split(" ").take(2).reverse.toList
    }.toList

    val sorted = actual.zip(probabilityAndPrediction).map {
      case (actualClass, l2) => {
        val List(probability, predictedClass) = l2
        (probability, if (predictedClass == actualClass) 1 else 0)
      }
    }.sortBy(_._1)

    val oneThird = sorted.length / 3

    val high = sorted.slice(oneThird * 2, sorted.length)
    println("High confidence accuracy: " + 100.0 * high.map(_._2).sum.toFloat / high.length)

    val mid = sorted.slice(oneThird, oneThird * 2)
    println("Mid confidence accuracy: " + 100.0 * mid.map(_._2).sum.toFloat / mid.length)

    val low = sorted.slice(0, oneThird)
    println("Low confidence accuracy: " + 100.0 * low.map(_._2).sum.toFloat / low.length)

  }  

}
