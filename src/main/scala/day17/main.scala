package day17

package day17

import scala.io.Source
import scala.annotation.tailrec

object Main {
    def main(args: Array[String]) = {
        val jet = Source
            .fromResource("days11_20/day17_input.txt")
            .mkString
            .toArray

        val shapes = Array[Array[Int]](
            Grid.lineLR, Grid.plus, Grid.el,
            Grid.lineUD, Grid.square
        )
        val nextShapeFn = Helpers.makeLoopedItr(shapes)
        val nextJetFn = Helpers.makeLoopedItr(jet)
        val shaft = Grid.initialGrid()

        // test()

        // Calculated reference points for input
        // 2022 => 3098
        // 4044 => 6154
        // 6066 => 9249
        // 20220 => 30824

        var start = System.currentTimeMillis()
        val result2022 = runSimulation(shapes, jet, 2022, true)
        var end = System.currentTimeMillis()
        println(s"Part 1: Simulated at 2022 steps; Height of column: ${result2022}")
        println(s"Simulation completed in: ${end - start}ms")

        start = System.currentTimeMillis()
        val result1T = runSimulation(shapes, jet, 1000000000000l)
        end = System.currentTimeMillis()
        println(s"Part 2: Simulated at 1 trillion steps; Height of column: ${result1T}")
        println(s"Simulation completed in: ${end - start}ms")
    }


    def runSimulation(
        shapes: Array[Array[Int]],
        jetStream: Array[Char],
        desiredIterations: Long,
        doFull: Boolean = false
    ): Long = {
        // these are specific to my input
        // found by looking for a repeat in the output, then finding the start and length
        val numStepsToStartOfCycle = 95
        val numStepsInCycle = 1715
        val numRowsInCycle = 2616
        val offsetIterations = desiredIterations - numStepsToStartOfCycle
        val numFullCycles = offsetIterations / numStepsInCycle
        val stepsToEmulate = (offsetIterations % numStepsInCycle) + numStepsToStartOfCycle
        val fullCycleHeight = numFullCycles * numRowsInCycle

        val nextShape = Helpers.makeLoopedItr(shapes)
        val nextJet = Helpers.makeLoopedItr(jetStream)
        val shaft = Grid.initialGrid()

        val droppings = simulate(shaft, nextShape, nextJet, 0, stepsToEmulate, shaft.length - 1)   
        val shaftTotalHeight = droppings.length - 1        
        val topIndex = droppings.indexWhere(_ != 0)
        val emulatedHeight = shaftTotalHeight - topIndex

        fullCycleHeight + emulatedHeight
    }

    @tailrec
    def simulate(
        shaft: Array[Int],
        nextShape: ()=>Array[Int],
        nextJet: ()=>Char,
        step: Long,
        limit: Long,
        lowestPoint: Int
    ): Array[Int] = {
        if (step != limit) {
            val shape = nextShape().clone
            val dropFrom = lowestPoint - 1
            val renderSize = dropFrom - (shape.length-1)

            val (newShaft, growthOffset) = 
                if (renderSize >= 0) (shaft, 0)
                else {
                    val newShaft = Grid.extend(shaft)
                    val growth = newShaft.length - shaft.length
                    (newShaft, growth)
                }

            fastDrop(shape, nextJet)

            val (stoppedShape, stopIndex) = dropShape(newShaft, shape, nextJet, dropFrom+growthOffset)
            Grid.applyShape(newShaft, stoppedShape, stopIndex)
            val newLowestPoint = math.min(stopIndex - (stoppedShape.length-1), lowestPoint + growthOffset)
            simulate(newShaft, nextShape, nextJet, step+1, limit, newLowestPoint)
        } else {
            shaft
        }
    }

    @tailrec
    def dropShape(shaft: Array[Int], shape: Array[Int], nextJet: ()=>Char, shaftIndex: Int): (Array[Int], Int) = {
        val push = nextJet()

        val newShape = push match {
            case '>' => if (Helpers.canMoveRight(shape, shaft, shaftIndex)) moveRight(shape) else shape
            case '<' => if (Helpers.canMoveLeft(shape, shaft, shaftIndex)) moveLeft(shape) else shape
        }
        if (canMoveDown(newShape, shaft, shaftIndex+1)) {
            dropShape(shaft, newShape, nextJet, shaftIndex+1)
        } else {
            (newShape, shaftIndex)
        }
    }

    // fastDrop is a minor optimization that forgoes the first drop left/right check (will always pass)
    // plus ignores the matching against objects for the next two drops
    def fastDrop(shape: Array[Int], nextJet: ()=>Char) = {
        if (nextJet() == '<')
            moveLeftMut(shape)
        else
            moveRightMut(shape) 
            
        for ( _ <- 0 until 2) {
            nextJet() match {
                case '<' => if (canMoveLeftFast(shape)) moveLeftMut(shape)
                case _ => if (canMoveRightFast(shape)) moveRightMut(shape)
            }
        }
        shape
    }

    def moveLeft(shape: Array[Int]) = shape.map(v => v << 1)
    def moveRight(shape: Array[Int]) = shape.map(v => v >> 1)

    def canMoveLeftFast(shape: Array[Int]) = {
        val outOfBounds = shape.iterator
            .map(v => (v & 0x40) > 1 )
            .find(v=> v)
        outOfBounds.isEmpty
    }
    def canMoveRightFast(shape: Array[Int]) = {
        val outOfBounds = shape.iterator
            .map(v => (v & 0x1) == 1) // 0x01 is furthest right, so it can't move more right
            .find(v=> v)
        outOfBounds.isEmpty
    }


    def moveLeftMut(shape: Array[Int]) = {
        for (i <- 0 until shape.length) {
            shape(i) = shape(i) << 1
        }
        shape
    }
    def moveRightMut(shape: Array[Int]) = {
        for (i <- 0 until shape.length) {
            shape(i) = shape(i) >> 1
        }
        shape
    }

    def canMoveDown(shape: Array[Int], shaft: Array[Int], shaftIndex: Int) = {
        shape
            .reverseIterator
            .take(2) // to account for the 2nd layer of plus
            .zipWithIndex
            .map( entry =>
                shaft(shaftIndex - entry._2) & shape(shape.length - 1 - entry._2)
            )
            .find(x => x > 0)
            .isEmpty
    }


    // def test() = {
    //     var grid = Array[Int](
    //         0x0,
    //         0x0,
    //         0x0,
    //         0x0,
    //         0x127,
    //     )

    //     var shape = Grid.el
    //     println(Grid.drawShaftWithShape(grid, shape, 2))

    //     assert(canMoveDown(shape, grid, 2) == canMoveDown(shape, grid, 2))
    //     grid(3) = 0x10
    //     // println(Grid.drawShaftWithShape(grid, shape, 2))
    //     assert(canMoveDown(shape, grid, 2) == canMoveDown(shape, grid, 2))
    //     shape = Grid.plus
    //     assert(canMoveDown(shape, grid, 2) == canMoveDown(shape, grid, 2))

    //     grid(2) = 0x10
    //     println(Grid.drawShaftWithShape(grid, shape, 2))
    //     assert(canMoveDown(shape, grid, 2) == canMoveDown(shape, grid, 2))

    //     for (i <- 0 until 20) {
    //         var start = System.currentTimeMillis()
    //         (0 until 100000).foreach( x => canMoveDown(shape, grid, 2))
    //         var end = System.currentTimeMillis()
    //         println(s"original: ${end - start}")

    //         start = System.currentTimeMillis()
    //         (0 until 100000).foreach( x => canMoveDown(shape, grid, 2))
    //         end = System.currentTimeMillis()
    //         println(s"revised: ${end - start}")

    //     }
    // }
}
