@main def runDay(inputDir: String, day: Int): Unit =
  val s = scala.io.Source.fromFile(s"${inputDir}/day${day}.txt").getLines
  val out = day match {
    case 1 => day1(s.map(_.toInt).toList)
    case 2 => day2(s.toList)
  }
  println(out)
