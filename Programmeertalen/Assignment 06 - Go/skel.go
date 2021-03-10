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

	for i, row := range maze {
		for cell := range row {
			wall := maze[i][cell]

			if wall < 48 || wall > 51 {
				log.Fatal("Maze contains invalid characters.")
			}
		}
	}

	return
}

func solve(maze Maze) (route []Position) {
	var onceMaze [][]sync.Once

	explore := func(pos Position, path []Position) {
		if len(route) == 1 {

		}
	}

	onceMaze[0][0].Do(func() {
		go explore(pos, path)
	})

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

	return
}

func openPositions(pos Position, lastpos Position, maze Maze) (freePositions []Position) {
	north := Position{Row: pos.Row - 1, Col: pos.Col}
	if north.Row >= 0 && north != lastpos && maze[north.Row][north.Col]&southWall == 0 {
		freePositions = append(freePositions, north)
	}

	east := Position{Row: pos.Row, Col: pos.Col + 1}
	if east != lastpos && maze[pos.Row][pos.Col]&eastWall == 0 {
		freePositions = append(freePositions, east)
	}

	south := Position{Row: pos.Row + 1, Col: pos.Col}
	if south != lastpos && maze[pos.Row][pos.Col]&southWall == 0 {
		freePositions = append(freePositions, south)
	}

	west := Position{Row: pos.Row, Col: pos.Col - 1}
	if west.Col >= 0 && west != lastpos && maze[west.Row][west.Col]&eastWall == 0 {
		freePositions = append(freePositions, west)
	}

	return freePositions
}

func main() {
	amountOfArgs := len(os.Args)

	if amountOfArgs != 2 {
		log.Fatalf("Incorrect amount of given arguments. Expected arguments: %d, actual: %d.", 2, amountOfArgs)
	}

	f, error := os.Open(os.Args[1])

	if error != nil {
		log.Fatalf("There was an error opening %s: %s.", os.Args[1], error)
	}

	maze := readMaze(f)
	for _, pos := range solve(maze) {
		maze[pos.Row][pos.Col] |= (1 << 2) // The third flag
	}

	for _, line := range maze {
		// TODO: handle errors
		fmt.Println(string(line))
	}
}
