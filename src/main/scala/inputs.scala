package aoc2020

import scala.io.Source

case class Location(inputDir: String, day: Int)

def inputLines(using l: Location) = scala.io.Source.fromFile(s"${l.inputDir}/day${l.day}.txt").getLines

def simpleInput(solver: String => String)(using l: Location): String =
  solver(inputLines.mkString)

def input[A](format: String => A = identity)(solver: List[A] => String)(using l: Location): String =
  inputF(format)(List)(solver)

def inputF[A, C[_]](format: String => A = identity)(coll: collection.Factory[A, C[A]])(solver: C[A] => String)(using l: Location): String =
  solver(inputLines.map(format).to(coll))

def flatInput[A](format: String => List[A] = List.apply)(solver: List[A] => String)(using l: Location): String =
  flatInputF(format)(List)(solver)

def flatInputF[A, C[_]](format: String => IterableOnce[A] = identity)(coll: collection.Factory[A, C[A]])(solver: C[A] => String)(using l: Location): String =
  solver(inputLines.flatMap(format).to(coll))

def boolChar(trueChar: Char): String => Vector[Boolean] = _.map(_ == trueChar).toVector
