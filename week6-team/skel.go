package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sync"
)

const southWall byte = (1 << 0) // The first flag
const eastWall byte = (1 << 1)  // The second flag

type Maze [][]byte

type Position struct {
	Row, Col int
}

var channelsDone chan int = make(chan int)
var routes chan []Position = make(chan []Position)

/* EXAMPLE: for a Position pos, there is NO wall north of pos if and only if
 *  maze[pos.Row - 1][pos.Col] & southWall == 0
 *
 * Use the above construct, including the '== 0' part, when checking for the
 * absence of walls. For an explanation of why this works, see the provided
 * 'Questions and Answers' document.
 */

func readMaze(f *os.File) (maze Maze) {

	s := bufio.NewScanner(f)
	for s.Scan() {

		maze = append(maze, []byte(s.Text()))
	}

	for _, row := range maze { // is dit goed?
		for _, cell := range row {
			if cell < 0 || cell > 3 {

				log.Fatal("File contains one or more invalid characters.")
			}
		}
	}

	return
}

func walls(pos Position, maze Maze) []Position {

	var newP []Position = make([]Position, 0, 4)

	if maze[pos.Row-1][pos.Col]&southWall != 0 { // Noord
		newP = append(newP, Position{Row: pos.Row - 1, Col: pos.Col})
	}
	if maze[pos.Row][pos.Col]&southWall != 0 { // Zuid
		newP = append(newP, Position{Row: pos.Row + 1, Col: pos.Col})
	}
	if maze[pos.Row][pos.Col-1]&eastWall != 0 { // West
		newP = append(newP, Position{Row: pos.Row, Col: pos.Col - 1})
	}
	if maze[pos.Row][pos.Col]&eastWall != 0 { // Oost
		newP = append(newP, Position{Row: pos.Row, Col: pos.Col + 1})
	}

	return newP

	// if len(newP) - 1 == 0 { // can't progress
	// 	// end goroutine
	// } else if len(newP) - 1 == 1 { // only one possible path
	// 	// continue current goroutine
	// } else if len(newP) - 1 == 2 {
	// 	// spawn one extra goroutine, continue current goroutine
	// } else if len(newP) - 1 == 3 {
	// 	// // spawn two extra goroutines, continue current goroutine
	// }
}

func traverse(route []Position, maze Maze) []Position {

	for i := 0; i < len(route); i++ {

		var newP []Position = walls(route[len(route)-1], maze)

		if len(newP)-1 <= 0 { // can't progress
			// end goroutine
		} else if len(newP)-1 == 1 { // only one possible path
			// continue current goroutine
		} else if len(newP)-1 == 2 {
			// spawn one extra goroutine, continue current goroutine
		} else if len(newP)-1 == 3 {
			// // spawn two extra goroutines, continue current goroutine
		}
	}
}

func solve(maze Maze) []Position { // returns route

	var onceMaze [][]sync.Once

	var route []Position = make([]Position, 0)
	route = append(route, Position{Row: 0, Col: 0})

	for _, row := range maze {
		for _, cell := range row {
			onceMaze[row][cell] = new(sync.Once)
		}
	}
	// spawn goroutines met benodigde waarden

	for {
		var newRoute []Position = <-routes

		if newRoute[len(newRoute)-1].Row > len(maze)-1 {
			route = newRoute
			break
		} else if newRoute[len(newRoute)-1].Col > len(maze[0])-1 {
			route = newRoute
			break
		}

		row := newRoute[len(newRoute)-1].Row
		col := newRoute[len(newRoute)-1].Col

		onceMaze.Do[row][col](func() {
			traverse(newRoute, maze)
		})
	}

	// Initialize a channel for communication with goroutines
	// No functional dependency on the size of a buffer is allowed

	// Include a closure for the exploration of maze cells in goroutines
	/* NOTE: The closure will be a short-lived function. Normally, this
	 *       would be considered inefficient. However, achieving  efficient
	 *       concurrency is not the point of this exercise.
	 */

	// Initialize onceMaze and use it to limit each cell to a single visit

	// Start the exploration of the maze in a goroutine at position {0, 0}

	// Receive messages from the goroutines and spawn new ones as needed
	// Do not spawn new goroutines if a way out of the maze has been found
	// Stop receiving only when no more exploration goroutines are running

	return route
}

func main() {
	f, err := os.Open(os.Args[1])

	if err != nil {
		log.Fatal(err)
	}

	maze := readMaze(f)
	for _, pos := range solve(maze) {
		maze[pos.Row][pos.Col] |= (1 << 2) // The third flag
	}
	for _, line := range maze {
		// TODO: handle errors ??
		fmt.Println(string(line))
	}
}
