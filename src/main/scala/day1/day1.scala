package day1
import scala.io.Source
import util.control.Breaks._

def part1(): Unit = {
  val file = Source.fromResource("day1/p1.txt")
  var sum: Int = 0
//  for line <- file.getLines() do
//    var num = 0
//    breakable {
//      for c <- line do
//        if c.isDigit then
//          num += c.asDigit * 10
//          break
//    }
//
//    breakable {
//      for c <- line.reverse do
//        if c.isDigit then
//          num += c.asDigit
//          break
//    }
//    sum += num

  // alternative
//  for line <- file.getLines() do
//    val numbers = line.filter(c => c.isDigit).map(c => c.asDigit)
//    sum += numbers.head * 10 + numbers.last

  // alternative:
//  for line <- file.getLines() do
//    sum += line.findLast(c => c.isDigit).get.asDigit + line.find(c => c.isDigit).get.asDigit * 10

  // alternative
  sum = file.getLines().map(l => {
    l.findLast(c => c.isDigit).get.asDigit + l.find(c => c.isDigit).get.asDigit * 10
  }).sum

  println(s"part 1: $sum") // 54561

  file.close()
}

val stringToNumber = Map(
  "one" -> 1,
  "two" -> 2,
  "three" -> 3,
  "four" -> 4,
  "five" -> 5,
  "six" -> 6,
  "seven" -> 7,
  "eight" -> 8,
  "nine" -> 9
)

def parseOut(s: String, i: Int): Option[Int] = {
  if s(i).isDigit then
    Some(s(i).asDigit)
  else
    for ((k, v) <- stringToNumber) do
      if (k.length + i <= s.length && s.slice(i, i + k.length) == k) then
        return Some(v)
    None
}

def part2(): Unit = {
  val file = Source.fromResource("day1/p1.txt")
  var sum: Int = 0

  for line <- file.getLines() do
    val n = (0 until line.length).map(parseOut(line, _)).collectFirst({case Some(x) => x}).get * 10
    + (line.length - 1 until -1 by -1).map(parseOut(line, _)).collectFirst({case Some(x) => x}).get
    sum += n

  println(s"part 2: $sum") // 54076

  file.close()
}