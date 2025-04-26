package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

// The prune operation specified by the problem.
func prune(num int) int {
	return num % 16_777_216
}

// The mix operation specified by the problem.
func mix(num, salt int) int {
	return num ^ salt
}

// Applies the secret operation as specified by the problem.
func applySecret(num int) int {
	// Multiply by 2^6 = 64.
	salt := num << 6
	secret := prune(mix(num, salt))

	// (Floor) divide by 2^5 = 32.
	salt = secret >> 5
	secret = prune(mix(secret, salt))

	// Multiply by 2^11 = 2048.
	salt = secret << 11
	secret = prune(mix(secret, salt))

	return secret
}

// Applies the secret operation n times to the given seed.
func nthSecret(seed, n int) int {
	secret := seed

	for range n {
		secret = applySecret(secret)
	}
	return secret
}

func main() {
	filename := os.Args[1]
	file, err := os.Open(filename)

	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	sum := 0

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		seed, err := strconv.Atoi(scanner.Text())

		if err != nil {
			log.Fatal(err)
		}

		sum += nthSecret(seed, 2000)
	}

	fmt.Println(sum)

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}
