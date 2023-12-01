package day1

import scala.io.Source
import scala.util.chaining.*
import java.nio.file.{Path, Paths}

def check_line(s: String): Int = {
    
    val cs = s.toCharArray

    // arr[2]
    def loop(st: Int, ed: Int, acc: Array[Int], index_set: Set[Int], checked_index: Set[Int]): Int = {
        if index_set.size == 2 then {
            val sorted = index_set.toArray.sorted
            val a1 = acc(sorted.head)
            val a2 = acc(sorted.last)
            return a1 * 10 + a2
        }

        println(s"$st $ed $index_set")
        var nst = st
        var ned = ed

        var n_index_set = index_set
        var n_c_set = checked_index

        if st >= ed then {

            if !checked_index(st) then {
                val c1 = Character.isDigit(cs(st))
                if c1 then {
                    acc(0) = Character.digit(cs(st), 10)
                    n_c_set += st
                    n_index_set += 0
                }
            }

            val res =
                n_c_set.size match {
                    case 1 => 
                        val v = acc(n_index_set.head) 
                        v * 10 + v
                    case _: Int => 
                        val sorted = n_index_set.toArray.sorted
                        acc(sorted.head) * 10 + acc(sorted.last)
                }
            return res
        }
        // check1

        if !n_c_set(st) then {
            val c1 = Character.isDigit(cs(st))
            if c1 then {
                n_index_set += 0
                acc(0) = Character.digit(cs(st), 10)
                n_c_set += st
            } else {
                nst += 1
            }
        }

        if !n_c_set(ed) then {
            val c2 = Character.isDigit(cs(ed))
            if c2 then {
                n_index_set += 1
                acc(1) = Character.digit(cs(ed), 10)
                n_c_set += ed
            } else {
                ned -= 1
            }
        }
        loop(nst, ned, acc, n_index_set, n_c_set)
    }
    loop(0, s.length-1, Array(0,0), Set.empty, Set.empty)
}

@main def solve_recursion(): Unit = {

    val path = Paths.get("res/day1.txt")
    val lines = Source.fromFile(path.toFile).getLines

//     val test_data = """1abc2
// pqr3stu8vwx
// a1b2c3d4e5f
// treb7uchet"""

//     val lines = test_data.split("\n").map(_.trim)

    val res = 
        lines.zipWithIndex.map { case (a, index) => 
            val v = check_line(a)  
            println(s"index: ${index}   $v")
            v  
        }.sum

    println(res)
}

@main def solve_imper(): Unit = {
    val path = Paths.get("res/day1.txt")
    val lines = Source.fromFile(path.toFile).getLines


    def get_num(s: String): Vector[Int] = {
        val cs = s.toCharArray()
        var res = Vector.empty[Int]
        for c <- cs do {
            val g = Character.isDigit(c)
            if g then {
                res :+= Character.digit(c, 10)
            }
        }
        res
    }

    def correct_num(v: Vector[Int]): Int = {
        v.size match {
            case 1 => v.head * 10 + v.head
            case _ => v.head * 10 + v.last
        }
    }

    val res = 
        lines.map{v => 
            get_num(v) pipe correct_num
        }.sum
    
    println(res)
}

val map = Map(
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

val keys = map.keys.toList

def get_num_part2(s: String): Int = {

    def change_string_num(str: String, st: Int, ed: Int): String = {
        //println(s"checked : $str")

        val checked =
            var rrr = List.empty[(String, Int)]
            keys.foreach {v => 
                val contain = str.contains(v)
                if contain then {
                    val x = str.indexOf(v)
                    val x2 = str.lastIndexOf(v)
                    rrr = (v -> x) :: rrr
                    rrr = (v -> x2) :: rrr
                }
            }
            rrr

        if checked.isEmpty then return str
        
        val sorted = checked.sortBy(_._2)
        val h1 = sorted.head._2
        val h2 = sorted.last._2

        var nst = st
        var ned = ed
        var nstr= str

        if h1 < nst then { 
            nst = h1
            nstr = nstr.replaceFirst(sorted.head._1, s"${map(sorted.head._1)}")
        } 

        if h2 > ned then {
            ned = h2
            val k1 = sorted.last._2
            nstr = nstr.slice(0, k1) + s"${map(sorted.last._1)}"
        }

        if nstr == str then return str

        change_string_num(nstr, nst, ned)
    }

    // index search
    def search_index_st_ed(s1: String): (Int,Int) = {
        var res = Vector.empty[Int]
        for i <- 0 until s1.length do {
            if Character.isDigit(s1(i)) then {
                res :+= i
            }
        }
        res.size match {
            case 0 => s1.length-1 -> 0
            case 1 => res.head -> res.head
            case _ => res.head -> res.last
        }
    }

    val (st, ed) = search_index_st_ed(s)

    val update_str = change_string_num(s, st, ed)
    
    var res = Vector.empty[Int]
    for c <- update_str do {
        if Character.isDigit(c) then {
            res :+= Character.digit(c, 10)
        }
    }

    res.size match {
        case 1 => res.head * 10 + res.head
        case _ => res.head * 10 + res.last
    }

}

@main def test_2(): Unit = {
    val lines = """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen""".split("\n").map(_.trim)

    println(lines.map {v => 
        val g = get_num_part2(v)
        println(s"$v => $g"); g}.sum)
}

@main def solve_part_two(): Unit = {
    val path = Paths.get("res/day1.txt")
    val lines = Source.fromFile(path.toFile).getLines

    var sum_num = 0L
    lines.foreach {v => 
        val vv = get_num_part2(v).toLong 
        println(s"$v : $vv")
        sum_num += vv
    }
    
    println(sum_num)
}

@main def another_solution_for_study(): Unit = {
    val digitReprs = 
        map ++ (1 to 9).map(i => i.toString -> i)

    val digitReprRegex = digitReprs.keysIterator.mkString("|").r

    val line = "eightwothree"

    // key point is line.tails...
    
    val mats = 
        for 
            lt <- line.tails 
            om <- digitReprRegex.findPrefixOf(lt)
        yield 
            om
    
    val ms = mats.toList

    println(ms)
}