@main def runDay(inputDir: String, day: Int, part: Int = 1): Unit =
  val s = scala.io.Source.fromFile(s"${inputDir}/day${day}.txt").getLines
  val out = (day, part) match {
    case (1, 1) => day1_1(s.map(_.toInt).toList).toString
    case (1, 2) => day1_2(s.map(_.toInt).toList).toString
  }
  println(out)
