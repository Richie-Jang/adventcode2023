package day2

import scala.io.Source
import java.nio.file.{Path, Paths}
import scala.util.chaining.*


def getLinesFromFile(): Iterator[String] = {
    Source.fromFile(Paths.get("res/day2.txt").toFile).getLines()
}

enum Color {
    case R, G, B
}

object Color {
    def fromStr(s: String): Color = s match {
        case "blue" => B
        case "red" => R
        case _ => G
    }
}

import Color.*

case class Game(num: Int, cubes: Vector[(Int,Color)])

//12 red cubes, 13 green cubes, and 14 blue cubes
val storeCubes = Map(
    R -> 12, 
    G -> 13,
    B -> 14
)

// Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
def parseLine(s: String): Game = {
    val ss = s.split(':')
    val m1 = """Game (\d+)""".r.findFirstMatchIn(ss(0))
    val g = m1.get.group(1).toInt
    val pat2 = """(\d+) ([a-z]+)""".r
    val mat2 = pat2.findAllMatchIn(ss(1))
    val cubes = 
        mat2.map{ m => 
            val i = m.group(1).toInt
            val c = m.group(2) pipe Color.fromStr
            i -> c
        }.toVector
    Game(g, cubes)
}

val testCode1 = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""


def getLinesFromStr(s: String): Iterable[String] = {
    s.split("\n").map(_.trim)
}

@main def testPart1(): Unit = {
    val lines = getLinesFromStr(testCode1)
    val games = 
        lines.map { l => 
            parseLine(l)   
        }
    val res = 
        games.filter { g => 
            g.cubes.forall { v => 
                val limit = storeCubes(v._2) 
                v._1 <= limit
            }    
        }.map(_.num).sum
    println(res)
}

@main def solvePart1(): Unit = {
    val lines = getLinesFromFile()
    val games = 
        lines.map { l => 
            parseLine(l)   
        }
    val res = 
        games.filter { g => 
            g.cubes.forall { v => 
                val limit = storeCubes(v._2) 
                v._1 <= limit
            }    
        }.map(_.num).sum
    println(res)
}

val testCode2 = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""

def checkRuleForPart2(g: Game): Vector[Int] = {
    val gg = 
        g.cubes.groupBy(_._2)
            .map{kv =>
                val vs = kv._2
                kv._1 -> vs.map(_._1).sorted
            }
    gg.map(kv => kv._2.last).toVector
}

def computePower(vs: Vector[Int]): Int = {
    vs.foldLeft(1)(_*_)
}

@main def testSolvePart2(): Unit = {
    val lines = getLinesFromStr(testCode2)
    val games = lines.map(parseLine).toList

    val values = games.map(checkRuleForPart2)
    println(values.map(computePower).sum)
}

@main def solvePart2(): Unit = {
    val lines = getLinesFromFile()
    val games = lines.map(parseLine).toList

    val values = games.map(checkRuleForPart2)
    println(values.map(computePower).sum)
}