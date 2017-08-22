import scala.io.Source

//object x {
//  val in = Source.fromURL("https://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")
//
//  val words = in.getLines.toList filter (w => w forall (chr => chr.isLetter))
//
//  val mnem = Map(
//    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
//    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
//  )
//
//  val charCode: Map[Char, Char] =
//    for ((digit, str) <- mnem; ltr <- str) yield ltr -> digit
//
//  def wordCode(word: String): String =
//    word.toUpperCase map charCode
//
//  val wordsForNum: Map[String, Seq[String]] =
//    words groupBy wordCode withDefaultValue Seq()
//
//  def encode(number: String): Set[List[String]] =
//    if (number.isEmpty) Set(List())
//    else {
//      for {
//        split <- 1 to number.length
//        word <- wordsForNum(number take split)
//        rest <- encode(number drop split)
//      } yield word :: rest
//    }.toSet
//
//  def translate(number: String): Set[String] =
//    encode(number) map (_.mkString (" "))
//}

//x encode("7225247386")
//
//x translate("7225247386")

("7225247386" groupBy identity map (v => (v._1, v._2.length)) toList) sortBy (_._2)