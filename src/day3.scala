package day3

import scala.io.Source
import java.nio.file.{Path, Paths}
import scala.util.chaining.*


def getLinesFromFile(): Iterator[String] = {
    Source.fromFile(Paths.get("res/day3.txt").toFile).getLines()
}


case class Num(X: Range, Y: Int, intValue: Int) {
    def isAdjacent(sym: Sym): Boolean = {
        // check y
        val y_range = sym.y-1 to sym.y+1
        if y_range.contains(Y) then {
            val x_range = sym.x-1 to sym.x+1
            x_range.exists {x1 => 
                X.contains(x1)
            }
        } else {
            false
        }
    }
}

case class Sym(x: Int, y: Int, sign: Char)

def parseLine(s: String, y: Int): (List[Num], List[Sym]) = {
    var nums = List.empty[Num]
    var syms = List.empty[Sym]
    val strLen = s.length
    
    var started_index_x = -1
    var buf = List.empty[Char]

    def updateNums(x1: Int, y1: Int): Unit = {
        if buf.nonEmpty then 
            val n = Num((started_index_x to x1), y1, String(buf.reverse.toArray).toInt)
            nums = n :: nums
        end if
        buf = List.empty
        started_index_x = -1
    }

    for 
        i <- 0 until s.length
        c = s(i)
    do {
        if c.isDigit then {
            if started_index_x == -1 then {
                // start
                started_index_x = i
                buf = c :: List.empty[Char]
            } else {
                // continue
                buf = c :: buf
            }
            if strLen-1 == i then {
                updateNums(i,y)
            }
        } else {
            updateNums(i-1,y)
            if c != '.' then {
                syms = Sym(i,y,c) :: syms
            }
        }
    }
    nums.reverse -> syms.reverse
}

import scala.collection.mutable

@main def solvePoart1(): Unit = {
    val lines = getLinesFromFile().toList
    val strLen = lines.size
    val nums = mutable.ListBuffer.empty[Num]
    val syms = mutable.ListBuffer.empty[Sym]
    lines.zipWithIndex.foreach{case(l, i) => 
        val (ns, ss) = parseLine(l, i)
        nums.addAll(ns)
        syms.addAll(ss)
    }

    var sum = 0
    val checks = 
        Array.fill(strLen)(mutable.ArrayBuffer.empty[Int])

    for n <- nums do {
        if syms.exists(p => n.isAdjacent(p)) then {
            sum += n.intValue
            checks(n.Y).addOne(n.intValue)
        }
    }
    println(sum)
    
}

@main def solvePart2(): Unit = {
    val lines = getLinesFromFile().toList
    val strLen = lines.size
    val nums = mutable.ListBuffer.empty[Num]
    val syms = mutable.ListBuffer.empty[Sym]
    lines.zipWithIndex.foreach{case(l, i) => 
        val (ns, ss) = parseLine(l, i)
        nums.addAll(ns)
        syms.addAll(ss)
    }

    var sum = 0
    for s <- syms do {
        val connects = nums.filter{n => n.isAdjacent(s) }
        if connects.size == 2 then {
            sum += connects.map(_.intValue).fold(1)(_*_)
        }
    }
    println(sum)
}