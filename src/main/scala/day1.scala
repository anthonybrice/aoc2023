import zio.*
import fastparse.*, NoWhitespace.*

import java.lang.Character.isDigit
import scala.io.Source

object Day1 extends ZIOAppDefault:
  override def run =
    for
      x <- ZIO.succeedBlocking(Source.fromResource("day1").getLines)
      p1 = part1(x)
      _ = println(p1)
    yield ()

  def part1(x: Iterator[String]) =
    x.map { s =>
      val firstDigit = s.find(isDigit).get
      val lastDigit = s.findLast(isDigit).get
      s"$firstDigit$lastDigit".toInt
    }.sum

  def part2(x: Iterator[String]) =
    x.map { s =>
      val firstDigit = firstNumFromLeft(s)
    }

  def parseA[$: P] = P("a")
  def zero[$: P] = P(IgnoreCase("zero").!.map(_ => 0) | CharIn("0").!.map(_.toInt))
  def one[$: P] = P(IgnoreCase("one").!.map(_ => 1) | CharIn("1").!.map(_.toInt))
  def two[$: P] = P(IgnoreCase("two").!.map(_ => 2) | CharIn("2").!.map(_.toInt))
  def three[$: P] = P(IgnoreCase("three").!.map(_ => 3) | CharIn("3").!.map(_.toInt))
  def four[$: P] = P(IgnoreCase("four").!.map(_ => 4)| CharIn("4").!.map(_.toInt))
  def five[$: P] = P(IgnoreCase("five").!.map(_ => 5) | CharIn("5").!.map(_.toInt))
  def six[$: P] = P(IgnoreCase("six").!.map(_ => 6) | CharIn("6").!.map(_.toInt))
  def seven[$: P] = P(IgnoreCase("seven").!.map(_ => 7) | CharIn("7").!.map(_.toInt))
  def eight[$: P] = P(IgnoreCase("eight").!.map(_ => 8) | CharIn("8").!.map(_.toInt))
  def nine[$: P] = P(IgnoreCase("nine").!.map(_ => 9) | CharIn("9").!.map(_.toInt))

  def zeroR[$: P] = P(IgnoreCase("orez").!.map(_ => 0) | CharIn("0").!.map(_.toInt))
  def oneR[$: P] = P(IgnoreCase("eno").!.map(_ => 1) | CharIn("1").!.map(_.toInt))
  def twoR[$: P] = P(IgnoreCase("owt").!.map(_ => 2) | CharIn("2").!.map(_.toInt))
  def threeR[$: P] = P(IgnoreCase("eerht").!.map(_ => 3) | CharIn("3").!.map(_.toInt))
  def fourR[$: P] = P(IgnoreCase("ruof").!.map(_ => 4) | CharIn("4").!.map(_.toInt))
  def fiveR[$: P] = P(IgnoreCase("evif").!.map(_ => 5) | CharIn("5").!.map(_.toInt))
  def sixR[$: P] = P(IgnoreCase("xis").!.map(_ => 6) | CharIn("6").!.map(_.toInt))
  def sevenR[$: P] = P(IgnoreCase("neves").!.map(_ => 7) | CharIn("7").!.map(_.toInt))
  def eightR[$: P] = P(IgnoreCase("thgie").!.map(_ => 8) | CharIn("8").!.map(_.toInt))
  def nineR[$: P] = P(IgnoreCase("enin").!.map(_ => 9) | CharIn("9").!.map(_.toInt))

//  def firstNumFromLeft[$: P] =
//    P(Start ~ !(zero | one | two | three | four | five | six | seven | eight | nine) ~ AnyChar.rep ~ (zero | one | two | three | four | five | six | seven | eight | nine) ~ AnyChar.rep ~ End)

  def firstNumFromLeft(x: String) =
    val ys = Seq(("0",0),("1",1),("2",2),("3",3),("4",4),("5",5),("6",6),("7",7),("8",8),("9",9),("zero",0),("one",1),("two",2),("three",3),("four",4),("five",5),("six",6),("seven",7),("eight",8),("nine",9))
    var z = (Integer.MAX_VALUE, Integer.MAX_VALUE)
    ys.foreach { (s,n) =>
      val idx = x.indexOf(s)
      if idx != -1 && idx < z._1 then z = (idx, n)
    }
    z._2
  println(firstNumFromLeft("qeightwo2xjvfkfiveone"))

  def firstNumFromRight(x: String) =
    val ys = Seq(("0",0),("1",1),("2",2),("3",3),("4",4),("5",5),("6",6),("7",7),("8",8),("9",9),("orez",0),("eno",1),("owt",2),("eerht",3),("ruof",4),("evif",5),("xis",6),("neves",7),("thgie",8),("enin",9))
    var z = (Integer.MAX_VALUE, Integer.MAX_VALUE)
    ys.foreach { (s, n) =>
      val idx = x.reverse.indexOf(s)
      if idx != -1 && idx < z._1 then z = (idx, n)
    }
    z._2
  println(firstNumFromRight("qeightwo2xjvfkfiveone"))

//  val Parsed.Success(value, successIndex) = parse("a", parseA(_))
//  assert(value == () && successIndex == 1)
//
//  val f@Parsed.Failure(label, index, extra) = parse("b", parseA(_))
//  assert(
//    label == ""
//    && index == 0
//    && f.msg == """Position 1:1, found "b""""
//  )
//
//  val foo = parse("qeightwo2xjvfkfiveone", firstNumFromLeft(_), verboseFailures = true)
//  println(foo)