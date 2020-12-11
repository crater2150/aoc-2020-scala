package aoc2020

import scala.io.Source

sealed trait InputSource
case class Location(inputDir: String, day: Int) extends InputSource
case class Fixed(lines: List[String]) extends InputSource

def inputLines(using src: InputSource): Iterator[String] = src match {
  case Location(inputDir, day) => scala.io.Source.fromFile(s"${inputDir}/day${day}.txt").getLines
  case Fixed(input) => input.iterator
}


def simpleInput(solver: String => String)(using l: InputSource): String =
  solver(inputLines.mkString)

def input[A](format: String => A = identity)(solver: List[A] => String)(using l: InputSource): String =
  inputF(format)(List)(solver)

def inputF[A, C[_]](format: String => A = identity)(coll: collection.Factory[A, C[A]])(solver: C[A] => String)(using l: InputSource): String =
  solver(inputLines.map(format).to(coll))

def flatInput[A](format: String => List[A] = List.apply)(solver: List[A] => String)(using l: InputSource): String =
  flatInputF(format)(List)(solver)

def flatInputF[A, C[_]](format: String => IterableOnce[A] = identity)(coll: collection.Factory[A, C[A]])(solver: C[A] => String)(using l: InputSource): String =
  solver(inputLines.flatMap(format).to(coll))

def boolChar(trueChar: Char): String => Vector[Boolean] = _.map(_ == trueChar).toVector
