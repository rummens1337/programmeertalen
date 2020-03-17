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

	for i, row := range maze {
		for cell := range row {

			wall := maze[i][cell]

			if wall <= 47 || wall >= 52 {
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

	newPos := Position{Row: pos.Row - 1, Col: pos.Col}

	if pos.Row-1 >= 0 && newPos != lastpos && maze[pos.Row-1][pos.Col]&southWall == 0 {
		newP = append(newP, newPos)
	}

	/* No south wall: */

	newPos = Position{Row: pos.Row + 1, Col: pos.Col}

	if newPos != lastpos && maze[pos.Row][pos.Col]&southWall == 0 {
		newP = append(newP, newPos)
	}

	/* No west wall: */

	newPos = Position{Row: pos.Row, Col: pos.Col - 1}

	if pos.Col-1 >= 0 && newPos != lastpos && maze[pos.Row][pos.Col-1]&eastWall == 0 {
		newP = append(newP, newPos)
	}

	/* No east wall: */

	newPos = Position{Row: pos.Row, Col: pos.Col + 1}

	if newPos != lastpos && maze[pos.Row][pos.Col]&eastWall == 0 {
		newP = append(newP, newPos)
	}

	return newP
}

/* Traverses through a route, and instructs the solve function to add more go-routines
if necessary. */

func traverse(route []Position, maze Maze) {

	/* PreviousPos is given with walls to leave the position out where the solver
	just came from. */

	var previousPos Position

	if len(route) == 1 {
		previousPos = Position{Row: -2, Col: -2}
	} else {
		previousPos = route[len(route)-2]
	}

	var newP []Position = walls(route[len(route)-1], previousPos, maze)

	/* If one of the new positions is out of bounds (or in other words, an exit
	has been found), the current route will be sent into the channel finalroute. */

	for pos := range newP {
		if newP[pos].Row >= len(maze) || newP[pos].Col >= len(maze[0]) {

			finalroute <- route
			return
		}
	}

	/* Checks the amount of paths and takes the appropiate action (f.e. spawning
	the needed number of go-routines if more than two positions are possible).
	The first if is a special case for (0,0), since the algorithm would normally
	consider this a dead end. */

	if len(route) == 1 && len(newP) == 1 {
		route = append(route, newP[0])
		routes <- route

	} else if len(newP) == 0 {

		// print(newP[0].Row)
		// print(",")
		// print(newP[0].Col)
		// print("\n")

		return

	} else if len(newP) == 1 {

		route = append(route, newP[0])
		routes <- route

	} else if len(newP) == 2 {

		var newRoute []Position = route

		route = append(route, newP[0])
		routes <- route

		newRoute = append(newRoute, newP[1])
		routes <- newRoute

	} else if len(newP) == 3 {

		var newRoute []Position = route
		var newnewRoute []Position = route

		route = append(route, newP[0])
		routes <- route

		newRoute = append(newRoute, newP[1])
		routes <- newRoute

		newnewRoute = append(newnewRoute, newP[2])
		routes <- newnewRoute
	}
	return
}

/* Solves the maze and spawns new go-routines for if needed. Returns the shortest
route. */

func solve(maze Maze) []Position {

	// var routes chan []Position = make(chan []Position)
	// var finalroute chan []Position = make(chan []Position)

	var route []Position = make([]Position, 0)
	route = append(route, Position{Row: 0, Col: 0})

	// Sync.once is nu correct WOOT

	numRows := len(maze)
	numCol := len(maze[0])

	var onceMaze [][]sync.Once = make([][]sync.Once, numRows)

	for i := 0; i < numRows; i++ {
		onceMaze[i] = make([]sync.Once, numCol)
	}

	onceMaze[0][0].Do(func() {
		go traverse(route, maze)
	})

	var found bool
	for {
		select {
		case x := <-finalroute:
			route = x
			found = true
			break // break out of switch
		default:

			println("test")

			var newRoute []Position = <-routes

			print(newRoute[len(newRoute)-1].Row)
			print(",")
			print(newRoute[len(newRoute)-1].Col)
			print("\n")

			row := newRoute[len(newRoute)-1].Row
			col := newRoute[len(newRoute)-1].Col

			onceMaze[row][col].Do(func() {
				go traverse(newRoute, maze)
			})
		}

		if found {
			break // break out of loop
		}
	}

	/* For loop below waits for all the goroutines to finish. */

	// OOK WACHTEN OP finalroute.

	println("YOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO")

	for i := range route {
		print(route[i].Row)
		print(",")
		print(route[i].Col)
		print("\n")
	}

	// for i := 0; i < countRoutines; i++ {
	// 	<-routes
	// }

	// Comments in skeleton:

	// Initialize a channel for communication with goroutines
	// No functional dependency on the size of a buffer is allowed

	// Include a closure for the exploration of maze cells in goroutines
	/* NOTE: The closure will be a short-lived function. Normally, this
	 *       would be considered inefficient. However, achieving  efficient
	 *       concurrency is not the point of this exercise.
	 */

	// Initialize onceMaze and use it to limit each cell to a single visit

	// Receive messages from the goroutines and spawn new ones as needed
	// Do not spawn new goroutines if a way out of the maze has been found
	// Stop receiving only when no more exploration goroutines are running

	close(routes)
	close(finalroute)

	return route
}

func main() {
	f, err := os.Open(os.Args[1])

	if err != nil {
		log.Fatal("There was an error opening the file", err)
	}

	maze := readMaze(f)

	for _, pos := range solve(maze) {
		maze[pos.Row][pos.Col] |= (1 << 2) // The third flag
	}

	for _, line := range maze {
		if len(line) <= 0 {
			log.Println("Tried printing an empty line.")
		}
		fmt.Println(string(line))
	}
}

/* TODO:
-Wachten op alle goroutines
-Bugfixing
-Unsolvable maze > print original maze.
*/

/* Gevorderd (alleen als algo werkt): Het gevonden pad is zeker het kortste pad, want
elk punt kan maar 1 keer worden bezocht. Op het moment dat een routine aankomt bij een
kruispunt wat al bezocht is, is er dus al een snellere route gevonden en zal de routine
termineren (= "snoeien" van de zoekboom). Met dit in het achterhoofd zal de snelste
route dus altijd eerder bij bepaalde kruispunten zijn dan andere routes en zal de
snelste route dus ook het snelst bij de uitgang komen. */
