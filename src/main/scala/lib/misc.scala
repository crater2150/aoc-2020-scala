package aoc2020.lib

import scala.collection.mutable

def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {self =>
  override def apply(key: I) = self.synchronized(getOrElseUpdate(key, f(key)))
}
