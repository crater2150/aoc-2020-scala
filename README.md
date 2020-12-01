# Advent of Code 2020 - Solved in Scala 3 / dotty

## Setup

Requires cats library built for Scala 3.0.0-M1, which is not yet published. To compile it yourself:

```
git clone -b v2.3.0 git://github.com/typelevel/cats.git
cd cats
sbt '++ 3.0.0-M1 publishLocal'
```

## Running

To run the code for a day:

```
sbt "run <input_dir> <day> <part>"
```
where
- `<input_dir>` is the path to a folder with AOC inputs named "day1.txt", "day2.txt", ...  
	Can be downloaded e.g. using [cargo-aoc](https://github.com/gobanos/cargo-aoc)
- `<day>` is the day to run
- `<part>` is either 1 or 2. Defaults to 1 if not given
1
