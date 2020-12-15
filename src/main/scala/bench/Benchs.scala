package aoc2020

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

object Input { val in = parse("2,1,10,11,0,6") }

class Benchs {
  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @OutputTimeUnit(TimeUnit.MICROSECONDS)
  def measureAvgTime = playMut(Input.in, 2020)
}
