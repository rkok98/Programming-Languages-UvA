/* Names: Thomas Vos, Michel Rummens
 * StudentID's: 12829501, 13108093
 * This file can solve mazes of any size, and theoretically prints the fastest
 * solution (see explanation at the end of the file), or the original maze if it
 * is unsolvable.
 */

package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sync"
)

const southWall byte = (1 << 0)
const eastWall byte = (1 << 1)

type Maze [][]byte

type Position struct {
	Row, Col int
}

/* Reads the maze from a file. If the file contains invalid characters,
the program will return an error message. */

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

/* Returns all free positions around one position, not counting the position
where the solver just came from. */

func walls(pos Position, lastpos Position, maze Maze) (freePositions []Position) {
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

/* Traverses through a route, and instructs the solve function to add more
go-routines if necessary. Also sends the final route to a separate channel and
sends a 1 to "allRoutines" when the current go-routine is finished. */

func traverse(route []Position, maze Maze, routes chan []Position,
	finalroute chan []Position, allRoutines chan int) {

	/* Walls receives the last position of the route and the one before that
	(previousPos), so it can skip the latter one while determining the possible
	new routes. For 0.0, previousPos is set to -2,-2 so that it can never
	influence the comparison. */

	var previousPos Position

	if len(route) == 1 {
		previousPos = Position{Row: -2, Col: -2}
	} else {
		previousPos = route[len(route)-2]
	}

	var freePositions []Position = walls(route[len(route)-1], previousPos, maze)

	/* If one of the new positions is out of bounds (as in an exit has been
	found), the current route will be sent into the channel finalroute. */

	for pos := range freePositions {
		if freePositions[pos].Row >= len(maze) || freePositions[pos].Col >= len(maze[0]) {

			routes <- route
			finalroute <- route

			allRoutines <- 1

			return
		}
	}

	/* Checks the amount of paths and instructs the solve function to spawn the
	correct amount of go-routines. */

	if len(freePositions) == 0 {

		allRoutines <- 1

		return

	} else if len(freePositions) == 1 {

		route = append(route, freePositions[0])
		routes <- route

	} else if len(freePositions) == 2 {

		var newRoute []Position = route

		route = append(route, freePositions[0])
		routes <- route

		newRoute = append(newRoute, freePositions[1])
		routes <- newRoute

	} else if len(freePositions) == 3 {

		var newRoute []Position = route
		var newnewRoute []Position = route

		route = append(route, freePositions[0])
		routes <- route

		newRoute = append(newRoute, freePositions[1])
		routes <- newRoute

		newnewRoute = append(newnewRoute, freePositions[2])
		routes <- newnewRoute
	}

	allRoutines <- 1

	return
}

/* Solves the maze and spawns new go-routines for if needed. Returns the
shortest route. */

func solve(maze Maze) []Position {

	/* Declaration and initialization of the three channels needed: "routes" for
	temporary routes, "finalroute" for the final route and "allRoutines" that
	keeps track of finished go-routines. The int "activeRoutines functions as
	a counter for the amount of active routines. */

	var routes chan []Position = make(chan []Position)
	var finalroute chan []Position = make(chan []Position)
	var allRoutines chan int = make(chan int)

	activeRoutines := 0

	/* Initializes the sync.Once, used to make sure every position
	is only visited exactly once. */

	var route []Position = make([]Position, 0)
	route = append(route, Position{Row: 0, Col: 0})

	numRows := len(maze)
	numCol := len(maze[0])

	var onceMaze [][]sync.Once = make([][]sync.Once, numRows)

	for i := 0; i < numRows; i++ {
		onceMaze[i] = make([]sync.Once, numCol)
	}

	/* Spawns the first go-routine for 0.0. */

	var noPathCopy = make([]Position, len(route))
	copy(noPathCopy, route)

	onceMaze[0][0].Do(func() {
		activeRoutines++
		go traverse(noPathCopy, maze, routes, finalroute, allRoutines)
	})

	var found bool = false

	/* In this loop, 3 things are checked: firstly, if "routes" has received a
	new route, a new go-routine is spawned for that route, and "activeRoutines"
	is increased by 1. Secondly, if "allRoutines" has received a 1 (which means
	a go-routine has finished), "activeRoutines" is decreased by one. Lastly, if
	"finalroute" has received a route, this route will be saved and the bool
	"found" will be set to true, so that no more routines are spawned. The loop
	ends when all routines have finished. */

	for activeRoutines > 0 {

		select {
		case temp := <-routes:

			if found == false {

				var newRoute = make([]Position, len(temp))
				copy(newRoute, temp)

				row := newRoute[len(newRoute)-1].Row
				col := newRoute[len(newRoute)-1].Col

				onceMaze[row][col].Do(func() {

					activeRoutines++
					go traverse(newRoute, maze, routes, finalroute, allRoutines)
				})
			}

		case <-allRoutines:
			activeRoutines--

		case x := <-finalroute:

			if found == false {

				var copyFinal = make([]Position, len(x))
				copy(copyFinal, x)

				route = copyFinal
				found = true
			}
		}
	}

	close(allRoutines)
	close(routes)
	close(finalroute)

	/* If no solution has been found, route is set to nil. */

	if found == false {

		route = nil
	}

	return route
}

/* Main function, processes the input (and gives an error if needed) and sends
the maze to the python script, which prints it. */

func main() {

	if len(os.Args) < 2 {
		log.Fatal("Too few arguments given.")
	}

	f, err := os.Open(os.Args[1])

	if err != nil {
		log.Fatal("There was an error opening the file.", err)
	}

	maze := readMaze(f)

	for _, pos := range solve(maze) {
		maze[pos.Row][pos.Col] |= (1 << 2)
	}

	for _, line := range maze {
		if len(line) <= 0 {
			log.Println("Tried printing an empty line.")
		}
		fmt.Println(string(line))
	}
}

/* Expert: the found path is theoretically the shortest one, since every point
can only be visisted once. If a routine arrives at the position of a crossroad
where another routine has already been, the latter is faster and the former
stops. The quickest route will therefore always "block" other possible routes,
which ensures that the solver always returns this route.

However, this may not always be the case due to influences from outside. Other
routes may be found earlier if the go-routines of the quickest path have to wait
(possibly if other programs need memory or processors as well), so to actually
obtain the quickest route, multiple tests are needed. This is especially visible
in the maze without any walls, where the solutions are similar but often not the
same, due to the reason I described above. */
