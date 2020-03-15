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
var finalroute chan []Position = make(chan []Position)

/* EXAMPLE: for a Position pos, there is NO wall north of pos if and only if
 *  maze[pos.Row - 1][pos.Col] & southWall == 0
 *
 * Use the above construct, including the '== 0' part, when checking for the
 * absence of walls. For an explanation of why this works, see the provided
 * 'Questions and Answers' document.
 */

/* Reads the maze from a file. If the file contains invalid characters, the program
will return an error message. */

func readMaze(f *os.File) (maze Maze) {

	s := bufio.NewScanner(f)
	for s.Scan() {

		maze = append(maze, []byte(s.Text()))
	}

	for _, row := range maze { // is dit goed?
		for cell := range row {
			if cell < 48 || cell > 51 {

				log.Fatal("File contains one or more invalid characters.")
			}
		}
	}

	return
}

/* Returns all free positions around one position, not counting the position where
the solver just came from. */

func walls(pos Position, lastpos Position, maze Maze) []Position {

	var newP []Position = make([]Position, 0, 4)

	/* No north wall: */
	if maze[pos.Row-1][pos.Col] & southWall == 0 & maze[pos.Row-1][pos.Col] != lastpos {
		newP = append(newP, Position{Row: pos.Row - 1, Col: pos.Col})
	}

	/* No south wall: */
	if maze[pos.Row][pos.Col] & southWall == 0 & maze[pos.Row][pos.Col] != lastpos {
		newP = append(newP, Position{Row: pos.Row + 1, Col: pos.Col})
	}

	/* No west wall: */
	if maze[pos.Row][pos.Col-1] & eastWall == 0 & maze[pos.Row][pos.Col-1] != lastpos {
		newP = append(newP, Position{Row: pos.Row, Col: pos.Col - 1})
	}

	/* No east wall: */
	if maze[pos.Row][pos.Col] & eastWall == 0 & maze[pos.Row][pos.Col] != lastpos {
		newP = append(newP, Position{Row: pos.Row, Col: pos.Col + 1})
	}

	return newP
}

/* Traverses through a route, and instructs the solve function to add more go-routines
if necessary. */

func traverse(route []Position, maze Maze) {

	for i := 0; i < len(route); i++ {

		var newP []Position = walls(route[len(route)-1], route[len(route)-2], maze)

		/* If one of the new positions is out of bounds (or in other words, an exit
		has been found), the current route will be sent into the channel finalroute. */

		for pos := range(newP) {
			if pos.Row > len(maze) || pos.Col > len(maze[0]) {
				finalroute <- route
				return
			}
		}

		/* Checks the amount of paths and takes the appropiate action (f.e. spawning
		more go-routines if more than two positions are possible). */

		if len(newP) <= 0 {
			return
		} else if len(newP) == 1 {
			route = append(route, newP[0])
			routes <- route

		} else if len(newP) == 2 {
			route = append(route, newP[0])
			routes <- route

			var newRoute []Position = route
			newRoute = append(newRoute, newP[1])
			routes <- newRoute

		} else if len(newP) == 3 {
			route = append(route, newP[0])
			routes <- route

			var newRoute []Position = route
			newRoute = append(newRoute, newP[1])
			routes <- newRoute

			var newnewRoute []Position = route
			newnewRoute = append(newnewRoute, newP[1])
			routes <- newnewRoute
		}
	}

	return
}

/* Solves the maze and spawns new go-routines for if needed. */

func solve(maze Maze) []Position { // returns route

	var onceMaze [][]sync.Once

	var route []Position = make([]Position, 0)
	route = append(route, Position{Row: 0, Col: 0})

	for _, row := range maze {
		for cell := range row {
			var idk Maze =
			onceMaze[row][cell] = new(sync.Once)
		}
	}
	// spawn goroutines met benodigde waarden

	for {
		var newRoute []Position = <-routes

		row := newRoute[len(newRoute)-1].Row
		col := newRoute[len(newRoute)-1].Col

		onceMaze.Do[row][col](func() {
			traverse(newRoute, maze)
		})

		// als finalroute wat heeft, break
	}

	// wacht op remaining go routines

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
