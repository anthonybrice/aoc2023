import zio.*
import zio.Console.printLine

import java.lang.Character.isDigit
import scala.io.Source

object Day1 extends ZIOAppDefault:
  override def run =
    for
      p1 <- part1
      _ = println(p1)
    yield ()

  def part1 =
    for
      foo <- ZIO.succeedBlocking(Source.fromResource("day1").getLines)
      ints = foo.map { s =>
        val firstDigit = s.find(isDigit).get
        val lastDigit = s.findLast(isDigit).get
        s"$firstDigit$lastDigit".toInt
      }
    yield ints.sum

  def part2 =
    for
      foo <- ZIO.succeedBlocking(Source.fromResource("day1").getLines)
        ints = foo.map { s =>
          val firstDigit = s.find(isDigit).get
          val lastDigit = s.findLast(isDigit).get
          s"$firstDigit$lastDigit".toInt
        }
    yield 0
