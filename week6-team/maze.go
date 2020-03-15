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

func walls(pos Position, maze Maze) []Position {

	var newP []Position = make([]Position, 0, 4)

	/* No north wall: */
	if pos.Row-1 > 0 && maze[pos.Row-1][pos.Col]&southWall == 0 {
		newP = append(newP, Position{Row: pos.Row - 1, Col: pos.Col})
	}

	/* No south wall: */
	if maze[pos.Row][pos.Col]&southWall == 0 {
		newP = append(newP, Position{Row: pos.Row + 1, Col: pos.Col})
	}

	/* No west wall: */
	if pos.Col-1 > 0 && maze[pos.Row][pos.Col-1]&eastWall == 0 {
		newP = append(newP, Position{Row: pos.Row, Col: pos.Col - 1})
	}

	/* No east wall: */
	if maze[pos.Row][pos.Col]&eastWall == 0 {
		newP = append(newP, Position{Row: pos.Row, Col: pos.Col + 1})
	}

	return newP
}

/* Traverses through a route, and instructs the solve function to add more go-routines
if necessary. */

func traverse(route []Position, maze Maze) {

	for i := 0; i < len(route); i++ {

		var newP []Position = walls(route[len(route)-1], maze)

		/* If one of the new positions is out of bounds (or in other words, an exit
		has been found), the current route will be sent into the channel finalroute. */

		for pos := range newP {
			if newP[pos].Row > len(maze) || newP[pos].Col > len(maze[0]) {
				finalroute <- route
				return
			}
		}

		/* Checks the amount of paths and takes the appropiate action (f.e. spawning
		the needed number of go-routines if more than two positions are possible). */

		if len(newP) <= 1 {
			return
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
			newnewRoute = append(newnewRoute, newP[2])
			routes <- newnewRoute

		} else if len(newP) == 4 {
			route = append(route, newP[0])
			routes <- route

			var newRoute []Position = route
			newRoute = append(newRoute, newP[1])
			routes <- newRoute

			var newnewRoute []Position = route
			newnewRoute = append(newnewRoute, newP[2])
			routes <- newnewRoute

			var newnewnewRoute []Position = route
			newnewnewRoute = append(newnewnewRoute, newP[3])
			routes <- newnewnewRoute
		}
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
	countRoutines := 0

	numRows := len(maze)
	var onceMaze [][]sync.Once
	for i := 0; i < numRows; i++ {
		onceMaze = append(onceMaze, *new([]sync.Once))
	}

	go traverse(route, maze)

	for {
		select {
		case x, ok := <-finalroute:
			if ok {
				route = x
				break
			}
		default:
			var newRoute []Position = <-routes

			row := newRoute[len(newRoute)-1].Row
			col := newRoute[len(newRoute)-1].Col

			onceMaze[row][col].Do(func() {
				countRoutines++
				go traverse(newRoute, maze)
			})
		}
	}

	/* For loop below waits for all the goroutines to finish. */

	for i := 0; i < countRoutines; i++ {
		<-routes
	}

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
-Last error TODO: BELANGRIJK!
-Bugfixing: BELANGRIJK!
-Unsolvable maze > print original maze: bij tijd over.
*/

/* Gevorderd (alleen als algo werkt): Het gevonden pad is zeker het kortste pad, want
elk punt kan maar 1 keer worden bezocht. Op het moment dat een routine aankomt bij een
kruispunt wat al bezocht is, is er dus al een snellere route gevonden en zal de routine
termineren (= "snoeien" van de zoekboom). Met dit in het achterhoofd zal de snelste
route dus altijd eerder bij bepaalde kruispunten zijn dan andere routes en zal de
snelste route dus ook het snelst bij de uitgang komen. */
