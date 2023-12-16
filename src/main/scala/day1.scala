import zio.*
import fastparse.*, NoWhitespace.*

import java.lang.Character.isDigit
import scala.io.Source

object Day1 extends ZIOAppDefault:
  override def run =
    for
      x <- ZIO.succeedBlocking(Source.fromResource("day1").getLines.toSeq)
      p1 = part1(x)
      _ = println(p1)
      _ = println(part2(x))
    yield ()

  def part1(x: Seq[String]) = 
    x.map { s =>
      val firstDigit = s.find(isDigit).get
      val lastDigit = s.findLast(isDigit).get
      s"$firstDigit$lastDigit".toInt
    }.sum

  def part2(x: Seq[String]) =
    x.map { s =>
      val firstDigit = firstNumFromLeft(s)
      val lastDigit = firstNumFromRight(s)
      s"$firstDigit$lastDigit".toInt
    }.sum

  def firstNumFromLeft(x: String) =
    val ys = Seq(("0",0),("1",1),("2",2),("3",3),("4",4),("5",5),("6",6),("7",7),("8",8),("9",9),("zero",0),("one",1),("two",2),("three",3),("four",4),("five",5),("six",6),("seven",7),("eight",8),("nine",9))
    var z = (Integer.MAX_VALUE, Integer.MAX_VALUE)
    ys.foreach { (s,n) =>
      val idx = x.indexOf(s)
      if idx != -1 && idx < z._1 then z = (idx, n)
    }
    z._2

  def firstNumFromRight(x: String) =
    val ys = Seq(("0",0),("1",1),("2",2),("3",3),("4",4),("5",5),("6",6),("7",7),("8",8),("9",9),("orez",0),("eno",1),("owt",2),("eerht",3),("ruof",4),("evif",5),("xis",6),("neves",7),("thgie",8),("enin",9))
    val xr = x.reverse
    var z = (Integer.MAX_VALUE, Integer.MAX_VALUE)
    ys.foreach { (s, n) =>
      val idx = xr.indexOf(s)
      if idx != -1 && idx < z._1 then z = (idx, n)
    }
    z._2