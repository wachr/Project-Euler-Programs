/* euler-prob-05.go
author: Ray Wach
date: 2014-10-03
info: Program to solve Project Euler problem 5 in Go.
*/

package main

import (
	"fmt"
	"math"
	"os"
)

// a map of base to power representing the prime factorization of an integer
type primeFactors map[uint]uint

func PrintUsage() {
	fmt.Println("Usage:", os.Args[0], "<unsigned_integer>")
}

func GetPrimeFactors(num uint) primeFactors {
	pf := make(primeFactors)

	n := num
	for b := uint(2); b <= n; b++ {
		for n%b == 0 {
			n /= b
			pf[b] += 1
		}
	}

	// fmt.Printf("Prime factors of %d:\n\t%v\n", num, pf)
	return pf
}

/* Build a map of prime factors by trail division for all numbers up to a value
passed in on the command line. */
func main() {
	if len(os.Args) != 2 {
		PrintUsage()
		os.Exit(2)
	}

	// Parse command line argument.
	var ceiling uint
	_, err := fmt.Sscan(os.Args[1], &ceiling)
	if err != nil {
		fmt.Printf("Failed to read arg: %+v\n", err)
		os.Exit(1)
	}

	/* Get the prime factorization of each number in the range and select the
	   largest exponent for each prime. */
	primes := make(primeFactors)
	for i := uint(2); i <= ceiling; i++ {
		for base, exponent := range GetPrimeFactors(i) {
			if primes[base] < exponent {
				primes[base] = exponent
			}
		}
	}
	// Get the product of the prime factors raised to their exponents.
	product := uint(1)
	fmt.Printf("%d", product)
	for base, exponent := range primes {
		factor := uint(math.Pow(float64(base), float64(exponent)))
		fmt.Printf(" * %d^%d", base, exponent)
		product *= factor
	}
	fmt.Printf("\n")

	// Print result.
	fmt.Printf("LCM of [1, %v]: %v\n", ceiling, product)
}
