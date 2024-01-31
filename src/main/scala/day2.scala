package day2

import scala.io.Source

// only 12 red cubes, 13 green cubes, and 14 blue cubes
val limit: Map[String, Int] = Map(
  "red" -> 12,
  "green" -> 13,
  "blue" -> 14,
)

def isValidSet(pulledSet: String): Boolean = {
  // 5 red, 5 green -> [[5, red], [5, green]]
  pulledSet.split(", ").view.map(_.split(" ")).forall(x => limit(x(1)) >= x(0).toInt)
}

def part1(): Unit = {
  val file = Source.fromResource("day2.txt")

  var sum = 0
  for (line, gameNumber) <- file.getLines().zipWithIndex do
    // +1 to consume space
    val rounds = line.slice(line.indexOf(':') + 2, line.length).split("; ")
    if rounds.view.forall(isValidSet) then
      sum += gameNumber + 1

  println(s"Part 1: $sum") // 2256

  file.close()
}

def part2(): Unit = {
  val file = Source.fromResource("day2.txt")
  var sum = 0

  for line <- file.getLines() do
    val minCount = scala.collection.mutable.Map[String, Int]()

    val rounds = line.slice(line.indexOf(':') + 2, line.length).split("; ") // ["5 blue, 1 red", ...]
    for round <- rounds do
      for setPart <- round.split(", ") do
        val parts = setPart.split(" ")
        val count = parts(0).toInt
        minCount.addOne(parts(1) -> Math.max(minCount.getOrElseUpdate(parts(1), count), count))
    sum += minCount.map((k, v) => v).product

  println(s"Part 2: $sum") // 74229
  file.close()
}