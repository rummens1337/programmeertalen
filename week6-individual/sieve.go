/**
 * @author Michel Rummens <13108093>
 *
 * This application filters prime numbers from 2 - n
 */

package main

import (
	"fmt"
	"os"
	"strconv"
	"sync"
	"time"
)

func main() {

	// Better way of handling program input (more idiomatic in golang I suppose?)
	// maxPtr := flag.Int("max", 35, "an integer value")
	// flag.Parse()

	if len(os.Args) <= 1 {
		println("Too short mate.")
		return
	}

	max, err := strconv.Atoi(os.Args[1])
	if(err != nil){
		println("Baby yoda refuses this input format.")
		return
	}

	numbers := make(map[int]int)
	for i := 2; i < max+1; i++ {
		numbers[i] = i
	}

	var wg sync.WaitGroup
	ch := make(chan map[int]int, len(numbers))
	ch <- numbers
	go sieve(ch, max, &wg)
	time.Sleep(1 * time.Millisecond) 
	// Small sleep so waitgroups are added in the recursive function.
	// Waiting also asures all goroutines are done before exiting.
	wg.Wait()
}

func sieve(ch chan map[int]int, max int, wg * sync.WaitGroup) {
	numbers := make(map[int]int)
	numbers = <-ch

	if len(numbers) == 0 {
		return
	}

	for i := 2; i < max+1; i++ {
		elem, ok := numbers[i]
		if ok {
			fmt.Println(elem)
			for j := 2; j < max+1; j++ {
				if j%elem == 0 {
					delete(numbers, j)
				}
			}
			break
		}
	}

	ch <- numbers
	wg.Add(1)
	go sieve(ch, max, wg)
	wg.Done()
}
