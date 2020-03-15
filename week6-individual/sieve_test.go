package main

import (
	"fmt"
	"os"
	"runtime"
	"testing"
)

/*
 * This check gives a rough indication for the goroutines.
 * You might have a little bit more or less due to the concurrent nature.
 * But, the maximum routines seen should be close to the expected one.
 */
func testDoSieve(t *testing.T, max string, eRoutines int) func(*testing.T) {
	return func(t *testing.T) {
		savestdout := os.Stdout
		// No error checking. Bad style, do not follow!!
		os.Stdout, _ = os.Open(os.DevNull)
		mockedArgs := make([]string, 2)
		maxRoutines := 0

		go func() {
			for {
				currRoutines := runtime.NumGoroutine()
				if currRoutines > maxRoutines {
					maxRoutines = currRoutines
				}
				if currRoutines < maxRoutines {
					return
				}
			}
		}()

		mockedArgs[0] = "./sieve"
		mockedArgs[1] = max
		os.Args = mockedArgs
		main()
		os.Stdout = savestdout

		fmt.Printf("Running: sieve %s\n", max)
		fmt.Printf("Maximum routines seen: %d\n", maxRoutines)
		fmt.Printf("Expected to see around: %d\n", eRoutines)
	}

}

func TestSieve(t *testing.T) {
	t.Run("200", testDoSieve(t, "200", 46))
	t.Run("100", testDoSieve(t, "100", 25))
}
