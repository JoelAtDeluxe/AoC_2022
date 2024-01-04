package day18

import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

object Main {
    def main(args: Array[String]) = {
        val points = Source.fromResource("days11_20/day18_input.txt")
            .getLines()
            .map(_.split(","))
            .map(_.map(_.toInt))
            .map(dataset => Point(dataset(0), dataset(1), dataset(2)))
            .toArray

        val start = System.currentTimeMillis()
        val exposedSides = plotPoints(points, 0)
        val mid = System.currentTimeMillis()
        val holePoints = findGapPoints(points, 0)
        val end = System.currentTimeMillis()

        println("execute time: ", (mid-start), " => ", (end - mid))

        println(s"Exposed sides: $exposedSides")
    }

    @tailrec
    def plotPoints(points: Array[Point], nextPoint: Int, exposedSides: Int=0): Int = {
        if (nextPoint >= points.length) {
            exposedSides
        }
        else {
            val thisPoint = points(nextPoint)
            val newSides = 6
            val newlyCoveredSides = points.take(nextPoint)
                .map(p => thisPoint.isAdjacent(p))
                .filter(p => p)
                .map(p => 2)
                .sum
            plotPoints(
                points,
                nextPoint+1,
                exposedSides + newSides - newlyCoveredSides)
        }
    }

    @tailrec
    def findGapPoints(points: Array[Point], nextPoint: Int, openPoints: Set[Point] = Set()): Set[Point] = {
        if (nextPoint >= points.length) {
            openPoints
        }
        else {
            val thisPoint = points(nextPoint)
            val freePoints = thisPoint.adjacencies().diff(points)
            val newOpenPoints = openPoints ++ freePoints
            findGapPoints(points, nextPoint+1, newOpenPoints)
        }
    }

    def connectIntoSurfaces(points: Array[Point], nextPoint: Int, surfaces: List[Surface]) = {
        
    }

    def plan() = {

        // fix a coordinate, and start building out cross sections
        // e.g. plot all 01,YY,ZZ
        // figure out what points are missing between the YY and ZZ lines
        // classify the gaps as either exterior or interior.
        // As we add more layers, we should close up some gaps, creating just interior regions
        //
        

    }

    def aStar(
        origin: Step[Point],
        target: Point,
        walls: List[Point],
        seen: ListBuffer[Step[Point]],
        possibleSteps: ListBuffer[Step[Point]],
    ): Boolean = {
        if (origin.node.equals(target)) {
            true  // found a connection
        }
        else {
            // 1. find adjacencies + filter out unreachable nodes
            val validMovement = origin.node.adjacencies.diff(walls)
            val nextSteps = validMovement
                .map(p => Point.toStep(p, target, origin.costToReach + 1))

            // 2. update seen list (add new point, remove old points if better path found)
            seen += origin
            val toAdd = nextSteps.map(step => {
                val index = seen.indexWhere(s => s.node == step.node)
                index match {
                    case -1 => Some(step)
                    case _ => {
                        if(seen(index).costToReach > step.costToReach) {
                            seen(index).costToReach = step.costToReach
                            Some(seen.remove(index))
                        }
                        else None
                    }
                }
            })
            .filter(_.isDefined)
            .map(_.get)
            
            // 3. update possible steps
            possibleSteps.appendAll(toAdd)
            if (possibleSteps.length == 0) {
                false
            }
            else {
                // 4. find the next best step
                var minStepIndex = 0
                var minCost = possibleSteps.head.totalCost()
                for ( step <- possibleSteps.tail.zipWithIndex ) {
                    if (step._1.totalCost() < minCost) {
                        minStepIndex = step._2
                        minCost = step._1.totalCost()
                    }
                }
                val nextStep = possibleSteps.remove(minStepIndex)

                aStar(nextStep, target, walls, seen, possibleSteps)
            }
        }
    }

    // def aStar(
    //     origin: Point,
    //     target: Point,
    // ) = {
    //     val dist = origin.manhattanDistance(target)
    //     aStar(origin, target, )
    // }

    // def aStar(
    //     origin: Point,
    //     target: Point,
    //     seen: List[Point],
    //     candidates: List[Point],
    //     path: List[Point],
    //     evaluate: (Point) => Int
    // ): Option[List[Point]] = {
    //     if (origin == target) {
    //         // goal -- done searching
    //         Some(path)
    //     } else if (candidates.length == 0) {
    //         // We couldn't find a path, all routes failed
    //         None
    //     } else {
            
    //     }

    //     // as I remember it:
    //     // 1. Find all valid next steps
    //     // 2. Check each next step to see if it's valid (walkable + shorter than previous walk)
    //     //   2.1 encode each remaining next step
    //     val possibleNextSteps = origin.adjacencies()
    //     possibleNextSteps.diff(seen)
    // }

    // def buildEval(target: Point, cannotWalk: List[Point]): (Point)=>Int = {
    //     def evaluate(p: Point): Int = {
    //         if (cannotWalk.contains(p)) {
    //             -1
    //         } else {
    //             p.manhattanDistance(target)
    //         }
    //     }
    //     evaluate
    // }
}



// object Step[T] {

// }

case class Point(x: Int, y:Int, z: Int) {
    def isAdjacent(that: Point) = {
        manhattanDistance(that) == 1
    }

    def adjacencies() = {
        Array[Point](
            Point(this.x + 1, this.y, this.z),
            Point(this.x - 1, this.y, this.z),
            Point(this.x, this.y + 1, this.z),
            Point(this.x, this.y - 1, this.z),
            Point(this.x, this.y, this.z + 1),
            Point(this.x, this.y, this.z - 1),
        )
    }

    def manhattanDistance(p: Point): Int = {
        val xDiff = math.abs(this.x - p.x)
        val yDiff = math.abs(this.y - p.y)
        val zDiff = math.abs(this.z - p.z)
        (xDiff + yDiff + zDiff)
    }
}

object Point {
    def toStep(p: Point, target: Point, costToReach: Int) = {
        Step(p, costToReach, p.manhattanDistance(target))
    }
}

class Surface() {
    var points: ArrayBuffer[Point] = ArrayBuffer()

    def isTouching(p: Point): Boolean = {
        val touchingPoint = points.iterator.find(_.isAdjacent(p))
        touchingPoint.isDefined
    }

    def addPoint(p: Point) = {
        points.append(p)
    }

    def connectSurface(s: Surface) = {
        points = points ++ s.points
    }

    def isSurfaceTouching(s: Surface): Boolean = {
        // there's probably a better way to cull obviously mismatched entries
        val connectinPoint = points.iterator.find(p =>s.isTouching(p))
        connectinPoint.isDefined
    }
}