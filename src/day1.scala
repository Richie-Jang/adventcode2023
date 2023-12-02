package day1

import scala.io.Source
import scala.util.chaining.*
import java.nio.file.{Path, Paths}
import scala.collection.Iterable

// clean code retry
val testCode1 = """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"""

def getLinesFromString(s: String): Iterable[String] = {
    s.split('\n').map(_.trim)
}

def getLinesFromFile(): Iterator[String] = {
    Source.fromFile(Paths.get("res/day1.txt").toFile).getLines()
}

def getDigitsFromString(s: String): Int = {
    val collects = s.filter{ c => c.isDigit }.map { c => c.asDigit }
    if collects.size == 1 then collects.head * 10 + collects.head else collects.head * 10 + collects.last
}

@main def solvePart1(): Unit = {
    //val lines = getLinesFromString(testCode1)
    val lines = getLinesFromFile()
    val res = lines.map(getDigitsFromString).sum
    println(res)
}

val digitMap = Map(
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

val testCode2 = """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"""

val digitStrs = digitMap.keys.toArray

def convertNumberFromString(s: String): Int = {
    // digitStr -> index
    var collect_set  = {
        var tmp_r = Set.empty[(String, Int)]
        digitStrs.foreach { v => 
            val index1 = s.indexOf(v)
            if index1 >= 0 then tmp_r += (v -> index1)
            val index2 = s.lastIndexOf(v)
            if index2 >= 0 then tmp_r += (v -> index2)    
        }
        tmp_r
    } 

    val all_num_list = {
        val collect_from_strs = {
            collect_set.toList.sortBy(_._2)
                .map{case (str,index) => index -> digitMap(str) }
        }
        val collect_nums_from_str = {
            for 
                (c, index) <- s.zipWithIndex 
                if c.isDigit
            yield 
                index -> c.asDigit
        }.toList
        (collect_nums_from_str ++ collect_from_strs).sortBy(_._1)
    }

    if all_num_list.size == 1 then {
        all_num_list.head._2 * 10 + all_num_list.head._2
    } else {
        all_num_list.head._2 * 10 + all_num_list.last._2
    }
    
}

@main def test_solve_part2(): Unit = {
    val lines = getLinesFromString(testCode2)
    var sum = 0
    lines.foreach { v => 
        convertNumberFromString(v) tap (a => sum += a) pipe println}
    println(s"SUM : $sum")
}

@main def solvePart2(): Unit = {
    val lines = getLinesFromFile()
    val total_sum = lines.map { v => 
        convertNumberFromString(v)    
    }.sum
    println(total_sum)
}