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
sbt "run <input_dir> <day> [sample]"
```

`<input_dir>` is the path to a folder with AOC inputs named "day1.txt", "day2.txt", ..., for example for the
inputs included in this repo, it would be `inputs/2020`.  
`<day>` is the day to run.  
If given, `sample` is the sample input to use. Put sample inputs into `input_dir`, naming scheme is `dayX-sampleY.txt`.
The sample inputs for 2020 from the instructions are included in this repo.


Inputs can be downloaded e.g. using [cargo-aoc](https://github.com/gobanos/cargo-aoc) or of course manually.
