import zio.*
import scala.io.Source
import scala.util.Try

enum Color:
  case Red(balls: Int) extends Color
  case Green(balls: Int) extends Color
  case Blue(balls: Int) extends Color

def colorAsGame(color: Color): Game =
  color match
    case Color.Red(balls) => Game(balls, 0, 0)
    case Color.Green(balls) => Game(0, balls, 0)
    case Color.Blue(balls) => Game(0, 0, balls)

def parseInt(raw: String): Option[Int] =
  try
    Some(raw.toInt)
  catch
    case e: Exception => None

def parseColor(raw: String): Option[Color] =
  raw.trim.split(" ") match
    case Array(num, "red") => parseInt(num).map(Color.Red.apply)
    case Array(num, "green") => parseInt(num).map(Color.Green.apply)
    case Array(num, "blue") => parseInt(num).map(Color.Blue.apply)
    case _ => None

final case class Game(red: Int, green: Int, blue: Int):
  def +(other: Game): Game =
    Game(math.max(red, other.red), math.max(green, other.green), math.max(blue, other.blue))
  
  def admits(other: Game): Boolean =
    other.red <= red && other.green <= green && other.blue <= blue

val emptyGame: Game = Game(0, 0, 0)

val maxGame: Game = Game(12, 13, 14)

def validGame(game: Game): Boolean = maxGame.admits(game)

object Day2 extends ZIOAppDefault:
  override def run = 
    for 
      x <- ZIO.succeedBlocking(Source.fromResource("day2").getLines.toSeq)
      games = x.map(parseLine)
      sumOfIds = games.zipWithIndex.filter((game, i) => validGame(game)).map((game, i) => i + 1).sum
      _ = println(sumOfIds)
      powers = games.map { case Game(r,g,b) => r*g*b }
      _ = println(powers.sum)
    yield ()

  def parseLine(s: String): Game = 
    val pulls: Array[Array[Color]] = s.split(':')(1).split(';').map(_.split(',')).map(_.flatMap(parseColor))
    val pullSummaries: Array[Game] = pulls.map(_.foldLeft(emptyGame)((acc, next) => acc + colorAsGame(next)))
    val gameSummary: Game = pullSummaries.fold(emptyGame)(_ + _)
    gameSummary
