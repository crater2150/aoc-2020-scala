package aoc2020.lib

/* for splitting input with separator lines */
extension [A](input: List[A])(using CanEqual[A,A])
  def split(separator: A, keepSeparator: Boolean = false): LazyList[List[A]] =
    input.span(_ != separator) match {
        case (Nil, Nil)              => LazyList()
        case (h,   Nil)              => LazyList(h)
        case (h,   tail as (_ :: t)) =>
          h #:: (if keepSeparator then tail else t).split(separator)
      }


/* Using -Yexplicit-nulls isn't really ready for use with the java standard
 * library. e.g. String doesn't have `@NotNull` annotations for its methods
 */
extension (s: String)
  def splitOnce(regex: String): Option[(String, String)] =
    s.split(regex, 2) match {
      case Array(a, b) => Some((a.nn, b.nn))
      case _ => None
    }

extension (s: String | UncheckedNull)
  def nn: String = s.asInstanceOf[String]

extension (s: Array[String | UncheckedNull] | UncheckedNull)
  def nn: List[String] = s.asInstanceOf[Array[String]].toList


extension [K,V,W](map: Map[K,V])
  def mapValuesS(f: V => W): Map[K, W] = map.view.mapValues(f).toMap
