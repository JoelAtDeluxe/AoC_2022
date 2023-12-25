package day17

package day17

import scala.io.Source
import scala.annotation.tailrec
import java.io.BufferedWriter
import java.io.FileWriter
import ujson.Bool

object StringMain {
    def main(args: Array[String]) = {
        val jet = Source
            .fromResource("days11_20/day17_input.txt")
            .mkString
            .toArray

        val shapes = Array[Array[Int]](
            Grid.lineLR, Grid.plus, Grid.el,
            Grid.lineUD, Grid.square
        )
        val nextShapeFn = makeLoopedItr(shapes)
        val nextJetFn = makeLoopedItr(jet)
        val shaft = Grid.initialGrid()       

        val droppings = simulate(shaft, nextShapeFn, nextJetFn, 0, 2022)
        val shaftTotalHeight = droppings.length - 1
        
        val topIndex = droppings.indexWhere(entry => {
            entry != 0
        })
        println(shaftTotalHeight - topIndex)

    }

    @tailrec
    def simulate(shaft: Array[Int], nextShape: ()=>Array[Int], nextJet: ()=>Char, step: Long, limit: Long): Array[Int] = {
        if (step != limit) {
            val shape = nextShape()
            var highestPoint = shaftHighestPoint(shaft)
            val shaftHeight = shaft.length
            val requiredHeight = (highestPoint - 4 - shape.length)
            
            val newShaft = 
                if (requiredHeight >= 0) shaft
                else Grid.extension ++ shaft
            val dropFrom = shaftHighestPoint(newShaft) - 4 // -1 extra to move above highest point

            val (stoppedShape, stopIndex) = dropShape(newShaft, shape, nextJet, dropFrom)
            Grid.applyShape(newShaft, stoppedShape, stopIndex)
            simulate(newShaft, nextShape, nextJet, step+1, limit)
        } else {
            shaft
        }
    }

    @tailrec
    def dropShape(shaft: Array[Int], shape: Array[Int], nextJet: ()=>Char, shaftIndex: Int): (Array[Int], Int) = {
        val push = nextJet()

        val newShape = push match {
            case '>' => if (canMoveRight(shape, shaft, shaftIndex)) moveRight(shape) else shape
            case '<' => if (canMoveLeft(shape, shaft, shaftIndex)) moveLeft(shape) else shape
        }
        if (canMoveDown(newShape, shaft, shaftIndex+1)) {
            dropShape(shaft, newShape, nextJet, shaftIndex+1)
        } else {
            (newShape, shaftIndex)
        }
    }

    def shaftHighestPoint(shaft: Array[Int]) = shaft.zipWithIndex.find(pair => pair._1 > 0).get._2
    def moveLeft(shape: Array[Int]) = shape.map(v => v << 1)
    def moveRight(shape: Array[Int]) = shape.map(v => v >> 1)

    def canMoveLeft(shape: Array[Int], shaft: Array[Int], shaftIndex: Int) = {
        val possibleShape = shape.map(v => v << 1)
        val overlaps = possibleShape.zipWithIndex.map(entry => {
            val overlap = shaft(shaftIndex - entry._2) & possibleShape(shape.length - 1 - entry._2)
            overlap > 0
        }).find(x => x)

        val outOfBounds = shape
            .map(v => (v & 0x40) > 1 ) // 0100 0000 is furtheest left (1000 0000 is wall), so can't move more left
            .find(v=> v)
        overlaps.isEmpty && outOfBounds.isEmpty
    }

    def canMoveRight(shape: Array[Int], shaft: Array[Int], shaftIndex: Int) = {
        val possibleShape = shape.map(v => v >> 1)
        val overlaps = possibleShape.zipWithIndex.map(entry => {
            val overlap = shaft(shaftIndex - entry._2) & possibleShape(shape.length - 1 - entry._2) 
            overlap > 0
        }).find(x => x)

        val outOfBounds = shape
            .map(v => (v & 0x1) == 1) // 0x01 is furthest right, so it can't move more right
            .find(v=> v)
        overlaps.isEmpty && outOfBounds.isEmpty
    }
    def canMoveDown(shape: Array[Int], shaft: Array[Int], shaftIndex: Int) = {
        val overlaps = shape.zipWithIndex.map(entry => {
            val overlap = shaft(shaftIndex - entry._2) & shape(shape.length - 1 - entry._2) 
            overlap > 0
        }).find(x => x)
        overlaps.isEmpty
    }

    def makeLoopedItr[T](list: Array[T]): () => T = {
        var count = 0
        def next(): T = {
            val rtn = list(count)
            count = (count + 1) % list.length 
            rtn
        }
        next
    }
}

object Grid {

    def applyShape(shaft: Array[Int], shape: Array[Int], shaftIndex: Int) = {
        for (i <- (0 until shape.length)) {
            shaft(shaftIndex - i) = shaft(shaftIndex - i) | shape(shape.length-1-i)
        }
    }

    def drawShaft(shaft: Array[Int], better: Boolean = false): String = {
        val thing = shaft
            .map(_.toBinaryString
                .reverse.padTo(7, '0').reverse
                .map(c => if (c == '0') '.' else '#')
                .mkString
                .mkString("|", "", "|")
            )
        thing(thing.length - 1) = "+-------+"
        thing.mkString("\n")
    }

    def drawShaftWithShape(shaft: Array[Int], shape: Array[Int], shaftIndex: Int): String = {
        // overlay shape
        val playShaft = shaft.clone
        applyShape(playShaft, shape, shaftIndex)
        drawShaft(playShaft)
    }

    def initialGrid(): Array[Int] = {
        Array[Int](
            0x00,  //x000|0000  | |.......
            0x00,  //x000|0000  | |.......
            0x00,  //x000|0000  | |.......
            0x7F,  //x111|1111  | |#######
        )  
    }

    val extension = Array[Int](
            0x00,  //x000|0000  | |.......
            0x00,  //x000|0000  | |.......
            0x00,  //x000|0000  | |.......
            0x00,  //x000|0000  | |.......
        )

    val lineUD = Array[Int](
            0x10,  //x001|0000  | |..#....
            0x10,  //x001|0000  | |..#....
            0x10,  //x001|0000  | |..#....
            0x10,  //x001|0000  | |..#....
        )
    val plus = Array[Int](
            0x08,  //x000|1000  |...#...
            0x1C,  //x001|1100  |..###..
            0x08,  //x000|1000  |...#...
        )
    val el = Array[Int](
        0x04,  //x000|0100  |....#..
        0x04,  //x000|0100  |....#..
        0x1C,  //x001|1100  |..###..
    )

    val lineLR = Array[Int](
        0x1E  //x001|1110  |..####.
    )  
    val square = Array[Int](
        0x18,  //x001|1000  |..##...
        0x18,  //x001|1000  |..##...
    )
}

