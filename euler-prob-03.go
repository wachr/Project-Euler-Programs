/* euler-prob-03.go
author: Ray Wach
date: 2014-10-02
info: Program to solve Project Euler problem 3 in Go.
*/

package main

import (
	"fmt"
	"os"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Println("Usage:", os.Args[0], "<unsigned_int64>")
		os.Exit(2)
	}
	var num uint64
	_, err := fmt.Sscan(os.Args[1], &num)
	if err != nil {
		fmt.Printf("Failed to read arg: %+v\n", err)
		os.Exit(1)
	}
	result := LargestPrimeFactor(num)
	fmt.Println(result)
}

func LargestPrimeFactor(n uint64) uint64 {
	for i := 2; uint64(i) < n; i++ {
		if n%uint64(i) == 0 {
			return LargestPrimeFactor(n / uint64(i))
		}
	}
	return n
}
