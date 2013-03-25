package appliednlp.classify

import scala.io

import chalk.lang.eng.PorterStemmer
import chalk.util.SimpleTokenizer
import nak.core.AttrVal


object TextClassify {
  def main(args: Array[String]) {
    val Array(newsTrainDir, newsTestDir) = args
    val vocab = extractTrain(newsTrainDir)
    val testDocs = extractTest(newsTestDir, vocab)
  }

  def output(fname: String, vocab: Set[String], docs: Array[(String, Map[String, Double])]) {
    val writer = new java.io.PrintWriter(new java.io.File(fname))

    docs.foreach {
      case (category, termFreq) => {
        vocab.foreach { term =>
          writer.print(term + "=" + termFreq(term) + ",")
        }
        writer.println(category)
      }
    }
  }

  def extractTrain(newsTrainDir: String): Set[String] = {
    val documents = extract(newsTrainDir)

    // All tokens occurring 600 (to limit the size of the problem since I'm
    // doing it so inefficiently) or more times in the training data
    val vocab = documents.map(_._2).flatMap(_.toList).map(_._1)
      .groupBy(x=>x).mapValues(x=>x.length).filter(_._2 >= 600).map(_._1).toSet
    output("text.train", vocab, documents)
    vocab
  }

  def extractTest(newsTestDir: String, vocab: Set[String]) {
    val documents = extract(newsTestDir)
    output("text.test", vocab, documents)
  }

  def extract(dir: String) : Array[(String, Map[String, Double])] = {
    val stemmer = new PorterStemmer
    val categoryDirs = new java.io.File(dir).listFiles

    val documents = categoryDirs.flatMap { categoryDir =>
      val categoryName = categoryDir.getName
      val files = categoryDir.listFiles
      files.map { file =>
        val tokenizer = SimpleTokenizer(io.Source.fromFile(file, "latin1").mkString)
        // toLowerCase and stemmer to decrease vocab size...
        val tokenCounts = tokenizer.map { token =>
          stemmer(token.toLowerCase)
        }.groupBy(x=>x).mapValues(x=>x.length)
        val numTokens = tokenCounts.map(_._2).sum
        val termFreq: Map[String, Double] = tokenCounts.mapValues(x => x.toDouble / numTokens)
        (categoryName, termFreq.withDefaultValue(0.0))
      }
    }

    documents
  }
}
