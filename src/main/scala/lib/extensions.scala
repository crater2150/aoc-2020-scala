package aoc2020.lib

/* for splitting input with separator lines */
extension [A](input: List[A])(using Eql[A,A])
  def split(separator: A, keepSeparator: Boolean = false): LazyList[List[A]] =
    input.span(_ != separator) match {
        case (Nil, Nil)              => LazyList()
        case (h,   Nil)              => LazyList(h)
        case (h,   tail as (_ :: t)) =>
          h #:: (if keepSeparator then tail else t).split(separator)
      }


/* Using -Yexplicit-nulls isn't really ready for use with the java standard
 * library. e.g. String doesn't have `@NotNull` annotations
 */
extension (s: String) 
  def splitNN(regex: String, limit: Int = 0): List[String] =
    s.split(regex, limit).asInstanceOf[Array[String]].toList

  def splitOnce(regex: String): Option[(String, String)] =
    s.split(regex, 2) match {
      case Array(a, b) => Some((a.asInstanceOf[String], b.asInstanceOf[String]))
      case _ => None
    }

  def substr(from: Int, to: Int): String =
    s.substring(from, to).asInstanceOf[String]
  def substr(from: Int): String =
    s.substring(from).asInstanceOf[String]
