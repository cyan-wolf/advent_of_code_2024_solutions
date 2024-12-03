package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

// Read a line (report) from the scanner.
func readLine(scanner *bufio.Scanner) []int {
	nums_str := strings.Split(scanner.Text(), " ")

	// Code below maps []string -> []int.
	nums := make([]int, len(nums_str))

	for i := range nums_str {
		n, err := strconv.Atoi(nums_str[i])
		if err != nil {
			log.Fatal(err)
		}

		nums[i] = n
	}

	return nums
}

// Determines whether a report is safe.
func reportIsSafe(report []int) bool {
	isInc := true
	isDec := true
	stepsAreValid := true

	for i := range len(report) - 1 {
		curr, next := report[i], report[i+1]

		if curr < next {
			isDec = false
		} else if curr > next {
			isInc = false
		}

		step := math.Abs(float64(next - curr))

		if step < 1 || step > 3 {
			stepsAreValid = false
		}
	}

	// A report is valid if it is:
	// 1) Either increasing or decreasing, and
	// 2) each step is between 1 and 3.
	return (isInc || isDec) && stepsAreValid
}

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	safeReports := 0

	for scanner.Scan() {
		report := readLine(scanner)

		if reportIsSafe(report) {
			safeReports++
		}
	}

	fmt.Println(safeReports)

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}
