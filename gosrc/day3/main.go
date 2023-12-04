package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

var (
	testCode1 = `467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..`

	testCode2 = `.....*.........424.......348........94...82.....*..........#25..*.....46*380...#......618.902.142.......972......$906...-....%96........482.
.....12.............................../.......340...46.756.....327..=.........897...........@....*435...*..........................*546.....
.................................185.....654........*.....*........772..+959............................581...13...918..388/....895.........
.90............915.......758.664*........../.......885...564../160................830.869...........474.......*.......#.....................
...*.....510....@........$..........$..............................813..795..........*....210........%......438..........786.......778.77...
....984.....%...............+..712...83..*....130..................+....*...545.............*......+.............../.727./....826......*....
................490......519../...........16....%...42.822..486......214..../...............985.480..............798....................249.
........369*........317*.........632...#.............=...*.$........................-703.............+341............88.....*659...@........
............595.........566.............847............456...................................182.........................791........533.....
`
)

type Num struct {
	startX, startY, endX int
	value                int
}

func (n *Num) IsAdjant(s Symbol) bool {
	// check y
	var checky = false
	for y := s.y - 1; y <= s.y+1; y++ {
		if n.startY == y {
			checky = true
			break
		}
	}
	if !checky {
		return false
	}
	for x := s.x - 1; x <= s.x+1; x++ {
		if n.startX <= x && x <= n.endX {
			return true
		}
	}
	return false
}

type Symbol struct {
	x, y int
	sign rune
}

var (
	numCharSet = map[rune]any{
		'0': nil,
		'1': nil,
		'2': nil,
		'3': nil,
		'4': nil,
		'5': nil,
		'6': nil,
		'7': nil,
		'8': nil,
		'9': nil,
	}
)

func parseLine(s string, y int) ([]Num, []Symbol) {
	num := make([]Num, 0)
	sym := make([]Symbol, 0)

	strLen := len(s)

	start_num_index_x := -1
	var chars []rune

	updateNum := func(endx, yy int) {
		if len(chars) > 0 {
			ss := string(chars)
			n, err := strconv.Atoi(ss)
			if err != nil {
				panic(fmt.Errorf("%s can not convert to int", ss))
			}
			num = append(num, Num{start_num_index_x, yy, endx, n})
		}
		start_num_index_x = -1
		chars = nil
	}

	for i, c := range s {
		// num handling
		if _, ok := numCharSet[c]; ok {
			if start_num_index_x == -1 {
				start_num_index_x = i
				chars = []rune{c}
			} else {
				chars = append(chars, c)
			}
			if strLen-1 == i {
				updateNum(i, y)
			}
		} else {
			updateNum(i-1, y)
			if c != '.' {
				sym = append(sym, Symbol{i, y, c})
			}
		}
	}

	return num, sym
}

func getLinesFromTestCode(s string) []string {
	res := make([]string, 0)
	for _, r := range strings.Split(s, "\n") {
		res = append(res, strings.TrimSpace(r))
	}
	return res
}

func getLinesFromFile() []string {
	res := make([]string, 0)
	sourceTxt := `../../res/day3.txt`
	f, er := os.Open(sourceTxt)
	if er != nil {
		panic(fmt.Errorf("file is not existed %s", sourceTxt))
	}
	defer f.Close()
	br := bufio.NewScanner(f)
	for br.Scan() {
		res = append(res, br.Text())
	}
	return res
}

func main() {
	//lines := getLinesFromTestCode(testCode2)
	lines := getLinesFromFile()
	lineCount := len(lines)

	numList := make([]Num, 0)
	symList := make([]Symbol, 0)

	for i, l := range lines {
		ns, ss := parseLine(l, i)
		//fmt.Println(l, ns, ss)
		numList = append(numList, ns...)
		symList = append(symList, ss...)
	}

	sum := 0

	partNums := make([][]int, lineCount)
	for i := 0; i < lineCount; i++ {
		partNums[i] = make([]int, 0)
	}

	for _, n := range numList {
		for _, s := range symList {
			if n.IsAdjant(s) {
				partNums[n.startY] = append(partNums[n.startY], n.value)
				sum += n.value
				break
			}
		}
	}
	fmt.Println(sum)

	// for i := 0; i < lineCount; i++ {
	// 	fmt.Println(lines[i], partNums[i])
	// }
}
