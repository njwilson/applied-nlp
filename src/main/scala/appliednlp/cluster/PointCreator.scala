package appliednlp.cluster

import scala.io.Source

import nak.cluster._
import nak.util.CollectionUtil._
import chalk.util.SimpleTokenizer

import org.apache.log4j.Logger
import org.apache.log4j.Level

/**
 *  Read data and produce data points and their features.
 *
 *  @param filename the name of the file containing the data
 *  @return a triple, the first element of which is a sequence of id's
 *     (unique for each row / data point), the second element is the sequence
 *     of (known) cluster labels, and the third of which is the sequence of
 *     Points to be clustered.
 */
trait PointCreator extends (String => Iterator[(String,String,Point)])

/**
 * A companion object to the PointCreator trait that helps select the
 * PointCreator corresponding to each string description.
 */
object PointCreator {
  def apply(description: String) = description match {
    case "standard" => DirectCreator
    case "schools" => SchoolsCreator
    case "countries" => CountriesCreator
    case "fed-simple" => new FederalistCreator(simple=true)
    case "fed-full" => new FederalistCreator(simple=false)
    case _ => throw new MatchError("Invalid point creator function: " + description)
  }
}

/**
 * Read data in the standard format for use with k-means.
 */
object DirectCreator extends PointCreator {

  def apply(filename: String) = {
    Source.fromFile(filename).getLines().map { line =>
      line.split("""\s+""") match {
        case Array(id, label, x, y) => {
          val point = Point(Vector(x.toDouble, y.toDouble))
          (id, label, point)
        }
      }
    }.toIterator
  }

}


/**
 * A standalone object with a main method for converting the achieve.dat rows
 * into a format suitable for input to RunKmeans.
 */
object SchoolsCreator extends PointCreator {

  def apply(filename: String) = {
    Source.fromFile(filename).getLines().flatMap { line =>
      val split = line.split("""\s+""")

      // Split school name (possible multiple words) and scores into separate arrays
      split.splitAt(split.length - 4) match {
        case (schoolArray, Array(r4, m4, r6, m6)) => {
          val schoolName = schoolArray.mkString("_")
          Array(makePoint(schoolName, "4", r4, m4), makePoint(schoolName, "6", r6, m6))
        }
      }
    }.toIterator
  }

  private def makePoint(school: String, grade: String, reading: String, math: String) =
    (school + "_" + grade + "th", grade, Point(Vector(reading.toDouble, math.toDouble)))

}

/**
 * A standalone object with a main method for converting the birth.dat rows
 * into a format suitable for input to RunKmeans.
 */
object CountriesCreator extends PointCreator {

  def apply(filename: String) = List[(String,String,Point)]().toIterator

}

/**
 * A class that converts the raw Federalist
 * papers into rows with a format suitable for input to Cluster. As part of
 * this, it must also perform feature extraction, converting the texts into
 * sets of values for each feature (such as a word count or relative
 * frequency).
 */
class FederalistCreator(simple: Boolean = false) extends PointCreator {

  def apply(filename: String) = List[(String,String,Point)]().toIterator

  /**
   * Given the text of an article, compute the frequency of "the", "people"
   * and "which" and return a Point per article that has the frequency of
   * "the" as the value of the first dimension, the frequency of "people"
   * for the second, and the frequency of "which" for the third.
   *
   * @param texts A sequence of Strings, each of which is the text extracted
   *              for an article (i.e. the "text" field produced by
   *              FederalistArticleExtractor).
   */
  def extractSimple(texts: IndexedSeq[String]): IndexedSeq[Point] = {
    Vector[Point]()
  }

  /**
   * Given the text of an article, extract features as best you can to try to
   * get good alignment of the produced clusters with the known authors.
   *
   * @param texts A sequence of Strings, each of which is the text extracted
   *              for an article (i.e. the "text" field produced by
   *              FederalistArticleExtractor).
   */
  def extractFull(texts: IndexedSeq[String]): IndexedSeq[Point] = {
    Vector[Point]()
  }

}

object FederalistArticleExtractor {
  /**
   * A method that takes the raw Federalist papers input and extracts each
   * article into a structured format.
   *
   * @param filename The filename containing the Federalist papers.
   * @return A sequence of Maps (one per article) from attributes (like
   *         "title", "id", and "text") to their values for each article.
   */
  def apply(filename: String): IndexedSeq[Map[String, String]] = {

    // Regex to identify the text portion of a document.
    val JustTextRE = (
      """(?s)\*\*\* START OF THIS PROJECT GUTENBERG.+""" +
      """\*\*\*(.+)\*\*\* END OF THIS PROJECT GUTENBERG""").r

    // Regex to capture different parts of each article.
    val ArticleRE = (
      """(?s)(\d+)\n+""" + // The article number.
      """(.+?)\n+""" + // The title (note non-greedy match).
      """((?:(?:For|From)[^\n]+?)?)\s+""" + // The publication venue (optional).
      """((?:(?:Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday).+\d\d\d\d\.)?)\n+""" + // The date (optional).
      """((?:MAD|HAM|JAY).+?)\n+""" + // The author(s).
      """(To the [^\n]+)""" + // The addressee.
      """(.+)""" // The text.
      ).r

    val book = io.Source.fromFile(filename).mkString
    val text = JustTextRE.findAllIn(book).matchData.next.group(1)
    val rawArticles = text.split("FEDERALIST.? No. ")

    // Use the regular expression to parse the articles.
    val allArticles = rawArticles.flatMap {
      case ArticleRE(id, title, venue, date, author, addressee, text) =>
        Some(Map("id" -> id.trim,
          "title" -> title.replaceAll("\\n+", " ").trim,
          "venue" -> venue.replaceAll("\\n+", " ").trim,
          "date" -> date.replaceAll("\\n+", " ").trim,
          "author" -> author.replaceAll("\\n+", " ").trim,
          "addressee" -> addressee.trim,
          "text" -> text.trim))

      case _ => None
    }.toIndexedSeq

    // Get rid of article 71, which is a duplicate, and return the rest.
    allArticles.take(70) ++ allArticles.slice(71, allArticles.length)
  }

}
