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

	// Initialize a channel for communication with goroutines
	// No functional dependency on the size of a buffer is allowed
	routes chan []Position := make(chan []Position)
	finalroute chan []Position := make(chan []Position)
	allRoutines chan int := make(chan int)

	route []Position := make([]Position, 0)
	route = append(route, Position{Row: 0, Col: 0})

	amountOfRows := len(maze)
	amountOfCols := len(maze[0])

	// Include a closure for the exploration of maze cells in goroutines
	/* NOTE: The closure will be a short-lived function. Normally, this
	 *       would be considered inefficient. However, achieving  efficient
	 *       concurrency is not the point of this exercise.
	 */
	explore := func(position Position, path []Position) {
		if len(path) != 1 {
			previous = route[len(route)-2]
		}

		openPositions := openPositions(position, previous, maze)

		for i := range openPositions {
			if openPositions[i].Row >= amountOfRows || openPositions[i].Col >= amountOfCols {
				routes <- route
				finalroute <- route
				allRoutines <- 1

				return
			}

			routes <- append(route, freePos[i])
		}

		allRoutines <- 1
		return
	}

	// Initialize onceMaze and use it to limit each cell to a single visit
	onceMaze = make([][]sync.Once, numRows)

	for i := 0; i < numRows; i++ {
		onceMaze[i] = make([]sync.Once, numCol)
	}

	// Start the exploration of the maze in a goroutine at position {0, 0}
	activeRoutines := 0

	onceMaze[0][0].Do(func() {
		activeRoutines++
		go explore(pos, path)
	})

	// Receive messages from the goroutines and spawn new ones as needed
	// Do not spawn new goroutines if a way out of the maze has been found
	// Stop receiving only when no more exploration goroutines are running

	close(allRoutines)
	close(routes)
	close(finalroute)

	return
}

func openPositions(position Position, lastPosition Position, maze Maze) (openPositions []Position) {
	north := Position{Row: position.Row - 1, Col: position.Col}
	if north.Row >= 0 && north != lastPosition && maze[north.Row][north.Col]&southWall == 0 {
		openPositions = append(openPositions, north)
	}

	east := Position{Row: position.Row, Col: position.Col + 1}
	if east != lastPosition && maze[position.Row][position.Col]&eastWall == 0 {
		openPositions = append(openPositions, east)
	}

	south := Position{Row: position.Row + 1, Col: position.Col}
	if south != lastPosition && maze[position.Row][position.Col]&southWall == 0 {
		openPositions = append(openPositions, south)
	}

	west := Position{Row: position.Row, Col: position.Col - 1}
	if west.Col >= 0 && west != lastPosition && maze[west.Row][west.Col]&eastWall == 0 {
		openPositions = append(openPositions, west)
	}

	return
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
