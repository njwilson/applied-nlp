package appliednlp.classify

import nak.core.AttrVal
import chalk.lang.eng.PorterStemmer


/**
 * An object that sets up the configuration for command-line options using
 * Scallop and returns the options, ready for use.
 */
object PpaFeaturesOpts {

  import org.rogach.scallop._
  
  def apply(args: Array[String]) = new ScallopConf(args) {
    banner("""
For usage see below:
	     """)
    val help = opt[Boolean]("help", noshort = true, descr = "Show this message")
    val verbose = opt[Boolean]("verbose")
    val bitstringsSource = opt[String]("bitstrings", descr = "File containing bitstrings")
    val extendedFeatures = opt[Boolean]("extended",short='e', descr="Use extended features.")
    val inputFile = trailArg[String]("inputfile", descr = "Input file to create features from.")
  }
}


/**
 * An application for extracting features from the PPA native format for 
 * classification.
 */
object PpaFeatures {

  /**
   * The main method -- do the work. Don't change it.
   */
  def main(args: Array[String]) {

    // Parse and get the command-line options
    val opts = PpaFeaturesOpts(args)
   
    val inputFile = opts.inputFile()

    val bitstrings = opts.bitstringsSource.get match {
      case Some(bitstringsSource) =>
        io.Source.fromFile(bitstringsSource).getLines.map { line =>
          val Array(word, bitstring) = line.split("\\s+")
          (word -> BitVector(bitstring))
        }.toMap

      case None => new collection.immutable.HashMap[String, BitVector]()
    }

    val featureExtractor =
      if (opts.extendedFeatures()) new ExtendedFeatureExtractor(bitstrings)
      else BasicFeatureExtractor

    io.Source.fromFile(inputFile).getLines.foreach { line =>
      val Array(id, verb, noun, prep, prepObj, attach) = line.split(" ")
      val features = featureExtractor(verb, noun, prep, prepObj)
      println(features.map(_.toString).mkString(",") + "," + attach)
    }

  }

}

/**
 * A trait for classes that can extract features from the information in
 * the PPA files.
 */
trait FeatureExtractor {
  
  /**
   * Given the verb, noun, preposition, and prepositional object,
   * create a set of AttrVal objects. (A "feature" is an attribute with a
   * value.) 
   */
  def apply(verb: String, noun: String, prep: String, prepObj: String): Iterable[AttrVal]
}

/**
 * The simplest feature extractor: each word gets a feature, where the 
 * attribute is the type of the word. 
 */
object BasicFeatureExtractor extends FeatureExtractor {

  override def apply(
    verb: String, noun: String, prep: String, prepObj: String): Iterable[AttrVal] = {
    List(
      AttrVal("verb", verb),
      AttrVal("noun", noun),
      AttrVal("prep", prep),
      AttrVal("prep_obj", prepObj))
  }

}

/**
 * An extended feature extractor. It is your job to fill this out further.
 */
class ExtendedFeatureExtractor(bitvectors: Map[String, BitVector])
  extends FeatureExtractor {

  lazy val stemmer = new PorterStemmer

  override def apply(
    verb: String, noun: String, prep: String, prepObj: String): Iterable[AttrVal] = {

    // Use the basic feature extractor to get the basic features (no need to 
    // duplicate effort and specify it again).
    val basicFeatures = BasicFeatureExtractor(verb, noun, prep, prepObj)

    // ----- Single word features -----
    val stemmedWords = basicFeatures.map { wordFeature =>
      AttrVal("stemmed_" + wordFeature.attr, stemmer(wordFeature.value))
    }
    val nounForms = List(
      AttrVal("noun_form", extractForm(noun)),
      AttrVal("prep_obj_form", extractForm(prepObj)))
    val suffixes = List(
      AttrVal("noun_suffix", extractSuffix(noun)),
      AttrVal("verb_suffix", extractSuffix(verb)),
      AttrVal("prep_obj_suffix", extractSuffix(prepObj)))
    val abbreviations = List(
      AttrVal("noun_abbrev", extractAbbreviation(noun)),
      AttrVal("prep_obj_abbrev", extractAbbreviation(prepObj)))
    val hyphens = List(
      AttrVal("verb_hyphen", extractHyphen(verb)),
      AttrVal("noun_hyphen", extractHyphen(noun)),
      AttrVal("prep_obj_hyphen", extractHyphen(prepObj)))
    val featuresFromSingleWords = stemmedWords ++ nounForms ++ suffixes ++ abbreviations ++ hyphens

    // ----- Pairs of selected single word features -----
    val prepWithSingleFeatures = featuresFromSingleWords.map { feature =>
      AttrVal("prep+" + feature.attr, prep + "+" + feature.value)
    }
    val stemmedBigrams = comboFeatures(stemmedWords, 2, 2)
    val stemmedTrigrams = comboFeatures(stemmedWords, 3, 3)
    val bothNounForms = comboFeatures(nounForms, 2, 2)
    val featuresFromPairs = prepWithSingleFeatures ++ stemmedBigrams ++ stemmedTrigrams ++ bothNounForms

    // ----- Selected "top N bits" sequences and combinations
    val selectedTopN = 1.to(32, 5) flatMap { n =>
      List(
        AttrVal("verb_top_" + n + "_bits", bitvectors(verb).keepTopBits(n).toInt.toString),
        AttrVal("noun_top_" + n + "_bits", bitvectors(noun).keepTopBits(n).toInt.toString),
        AttrVal("prep_obj_top_" + n + "_bits", bitvectors(prepObj).keepTopBits(n).toInt.toString))
    }
    val selectedTopPrepsWithSelectedTopN = selectedTopN.flatMap { feature =>
      10.to(32, 10) map { nPreps =>
        val attr = "prep_top_" + nPreps + "+" + feature.attr
        val value = bitvectors(prep).keepTopBits(nPreps).toInt.toString + "+" + feature.value
        AttrVal(attr, value)
      }
    }
    val featuresFromBitstrings = selectedTopPrepsWithSelectedTopN

    val extendedFeatures = featuresFromSingleWords ++ featuresFromPairs ++ featuresFromBitstrings

    // Return the features. You should of course add your features to basic ones.
    basicFeatures ++ extendedFeatures
  }

  def comboFeatures(features: Iterable[AttrVal], minSize: Int, maxSize: Int): Iterable[AttrVal] = {
    features.toSet.subsets.filter { subset =>
      subset.size >= minSize && subset.size <= maxSize
    }.map { toCombine =>
      // Combine this set of AttrVal objects
      val (names, vals) = toCombine.map { x => (x.attr, x.value) }.unzip
      AttrVal(names.mkString("+"), vals.mkString("+"))
    }.toList
  }

  def extractAbbreviation(word: String): String = {
    if (word.matches("""[a-zA-Z].*\.""")) "yes" else "no"
  }

  def extractHyphen(word: String): String = {
    if (word.matches(""".+-.+""")) "yes" else "no"
  }

  def topBits(name: String, word: String, topBits: Int): AttrVal = {
    AttrVal(name + "_top_" + topBits + "_bits", bitvectors(word).keepTopBits(topBits).toInt.toString)
  }

  def extractForm(word: String): String = {
    if (word.matches("""-?[0-9].*""")) {
      if (word.matches("""-?[0-9][^a-zA-Z]*""")) {
        "number"
      } else {
        "number_letter"
      }
    } else if (word.matches("""[A-Z].*""")) {
      if (word.matches("""[A-Z].*[A-Z].*""")) {
        "XX"
      } else {
        "Xx"
      }
    } else {
      "other"
    }
  }

  val suffixes = Vector("ing", "ogy", "ed", "s", "ly", "ion", "tion", "ity", "ies")

  def extractSuffix(word: String): String = {
    (suffixes.flatMap { suffix => if (word.endsWith(suffix)) Some(suffix) else None } ++ Vector("other")).take(1)(0)
  }

}

/**
 * This is an entirely cruddy, slow implementation of a bit vector,
 * not using any bitwise ops, etc., but it should suffice for this problem.
 *
 * And, yes, we are using Ints where it could be Booleans, and we could have
 * the wrong values in there, but this keeps it easy, and again, is sufficient
 * for this problem.
 * 
 * Feel free to add more capability to this if it helps you create better
 * features.
 */
class BitVector(bits: IndexedSeq[Int]) {

  /**
   * Get the bit value at the given index.
   */
  def apply(index: Int) = bits(index)

  /**
   * Get the integer value of the bits
   */
  lazy val toInt = Integer.parseInt(bits.mkString, 2)

  /**
   *  Keep the top bits up to the given index, and then make the remaining bits
   *  zero.
   */
  def keepTopBits(index: Int) =
    new BitVector(bits.take(index) ++ Vector.fill(bits.length - index)(0))

  /**
   * Concatenate the bits together.
   */
  override def toString = bits.mkString
}

/**
 * Companion object to the BitVector class.
 */
object BitVector {

  /**
   * Create a bit vector from a string of zeros and ones.
   */
  def apply(bitstring: String) =
    new BitVector(bitstring.split("").drop(1).map(_.toInt).toIndexedSeq)
}



