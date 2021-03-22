/**
 * Author:   René Kok (13671146)
 * Study:    Doorstroomminor Software Engineering UvA
 *
 * A distributed maze solver. The only argument for this program is to take the file name of a maze.
 *
 * A maze is a rectangular field of square boxes. These boxes have four sides
 * which are either open or blocked by a wall. One solution to the maze is to have
 * a series of connected squares such that a path runs from the entrance of the maze to the exit,
 * without the path being obstructed by a wall. The entrance is always on the north-west edge of
 * the maze, and the exit is on the south-east edge of the maze.
 */

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

// Maze is
type Maze [][]byte

// Position is
type Position struct {
	Row, Col int
}

/**
 * EXAMPLE: for a Position pos, there is NO wall north of pos if and only if
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
	var solution chan []Position = make(chan []Position)
	var routes chan []Position = make(chan []Position)
	var counter chan int = make(chan int)

	route = append(route, Position{Row: 0, Col: 0})

	amountOfRows := len(maze)
	amountOfCols := len(maze[0])

	// Include a closure for the exploration of maze cells in goroutines
	/* NOTE: The closure will be a short-lived function. Normally, this
	 *       would be considered inefficient. However, achieving  efficient
	 *       concurrency is not the point of this exercise.
	 */
	explore := func(position Position, route []Position) {
		var previous Position

		if len(route) == 1 {
			previous = Position{Row: -2, Col: -2}
		} else {
			previous = route[len(route)-2]
		}

		openPositions := openPositions(position, previous, maze)

		for i := range openPositions {
			if openPositions[i].Row >= amountOfRows || openPositions[i].Col >= amountOfCols {
				routes <- route
				solution <- route
				counter <- 1

				return
			}

			routes <- append(route, openPositions[i])
		}

		counter <- 1
		return
	}

	// Initialize onceMaze and use it to limit each cell to a single visit
	onceMaze = make([][]sync.Once, amountOfRows)

	for i := 0; i < amountOfRows; i++ {
		onceMaze[i] = make([]sync.Once, amountOfCols)
	}

	// Start the exploration of the maze in a goroutine at position {0, 0}
	start := Position{Row: 0, Col: 0}
	active := 0

	onceMaze[0][0].Do(func() {
		active++
		go explore(start, route)
	})

	// Receive messages from the goroutines and spawn new ones as needed
	// Do not spawn new goroutines if a way out of the maze has been found
	// Stop receiving only when no more exploration goroutines are running
	solved := false

	for active > 0 {
		select {
		case temp := <-routes:
			var route = make([]Position, len(temp))
			copy(route, temp)

			row := temp[len(temp)-1].Row
			col := temp[len(temp)-1].Col
			position := Position{Row: row, Col: col}

			onceMaze[row][col].Do(func() {
				active++
				go explore(position, route)
			})

		case <-counter:
			active--

		case x := <-solution:
			if !solved {

				var copyFinal = make([]Position, len(x))
				copy(copyFinal, x)

				route = copyFinal
				solved = true
			}
		}
	}

	close(counter)
	close(routes)
	close(solution)

	if !solved {
		route = nil
	}

	return
}

/**
 * Returns a list of every possible next step (a neighbor cell not seperated by a wall).
 */
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
		fmt.Println(string(line))
	}
}
