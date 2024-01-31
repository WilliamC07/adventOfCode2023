package day3
import scala.io.Source

def part1(): Unit = {
  val file = Source.fromResource("day3.txt")

  // could do Array[String], but chose 2D array to learn
  var matrix = Array[Array[Char]]()

  for line <- file.getLines() do
    matrix :+= line.toCharArray

  val R = matrix.length
  val C = matrix(0).length
  var sum = 0
  for
    r <- 0 until R
    c <- 0 until C
  do
    if matrix(r)(c).isDigit && (c == 0 || !matrix(r)(c - 1).isDigit) then
      var newC = c
      var number = 0
      var isValid = false
      while newC < C && matrix(r)(newC).isDigit do
        number *= 10
        number += matrix(r)(newC).asDigit
        // look at neighbor to determine if valid
        for
          aR <- -1 to 1
          aC <- -1 to 1
          if !isValid
        do
          val neighborR = aR + r
          val neighborC = aC + newC
          if neighborR >= 0 && neighborC >= 0 && neighborR < R && neighborC < C then
            val n = matrix(neighborR)(neighborC)
            if !n.isDigit && n != '.' then
              isValid = true
        newC += 1

      if isValid then
        sum += number

  println(s"part 1: $sum") // 540212
  file.close()
}

def parseNumberOut(r: Int, c: Int, matrix: Array[Array[Char]]): Int = {
  var number = 0
  var cPos = c

  while cPos >= 0 && matrix(r)(cPos).isDigit do
    cPos -= 1
  cPos += 1

  while cPos < matrix(r).length && matrix(r)(cPos).isDigit do
    number *= 10
    number += matrix(r)(cPos).asDigit
    cPos += 1

  number
}

def part2(): Unit = {
  val file = Source.fromResource("day3.txt")

  // could do Array[String], but chose 2D array to learn
  var matrix = Array[Array[Char]]()

  for line <- file.getLines() do
    matrix :+= line.toCharArray

  val R = matrix.length
  val C = matrix(0).length
  var sum = 0
  for
    r <- 0 until R
    c <- 0 until C
  do
    if matrix(r)(c) == '*' then
      var numbers = Array[Int]()
      // look at neighbor to determine if valid
      for
        aR <- -1 to 1
        aC <- -1 to 1
        if numbers.length <= 2
      do
        val neighborR = aR + r
        val neighborC = aC + c
        if neighborR >= 0 && neighborC >= 0 && neighborR < R && neighborC < C then
          val n = matrix(neighborR)(neighborC)
          if n.isDigit && (aC == -1 || !matrix(neighborR)(neighborC - 1).isDigit) then
            val v = parseNumberOut(neighborR, neighborC, matrix)
            numbers :+= v
        end if
      end for
      if numbers.length == 2 then
        sum += numbers.product
    end if
  end for

  println(s"part2: $sum") // 87605697
}
