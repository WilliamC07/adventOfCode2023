package day4
import scala.io.Source

def numWinningNumbers(line: String): Int = {
  val raw = line.slice(line.indexOf(':') + 2, line.length)
  val values = raw.split(" \\| ")
  val winningNums = values(0).split(" ").filter(_.nonEmpty).map(_.toInt).toSet
  val scratchedNums = values(1).split(" ").filter(_.nonEmpty).map(_.toInt).toSet
  winningNums.intersect(scratchedNums).size
}

def part1(): Unit = {
  val file = Source.fromResource("day4.txt")

  var score = 0
  for line <- file.getLines() do
    val matched = numWinningNumbers(line)
    if matched > 0 then
      score += 1 << (matched-1)


  println(s"Part1: $score") // 15205
  file.close()
}

def part2(): Unit = {
  val file = Source.fromResource("day4.txt")

  var scratchCardCount = 0
  val lines = file.getLines().toArray
  val numCardsOf = Array.fill(lines.length) {1}
  for
    (line, lineNumber) <- lines.iterator.zipWithIndex
    if numCardsOf(lineNumber) > 0
  do
    val count = numCardsOf(lineNumber)
    scratchCardCount += count
    val matched = numWinningNumbers(line)

    for nextCard <- (lineNumber to (lineNumber + matched)) do
      numCardsOf(nextCard) += count

  println(s"Part2: $scratchCardCount") // 6189740
  file.close()
}