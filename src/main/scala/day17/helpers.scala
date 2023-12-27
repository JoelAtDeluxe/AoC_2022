package day17

import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File

object Helpers {

    def makeLoopedItr[T](list: Array[T]): () => T = {
        var count = 0
        def next(): T = {
            val rtn = list(count)
            count = (count + 1) % list.length 
            rtn
        }
        next
    }

    def mkCanMove(shiftFn: (Int)=>Int, cmpFn: (Int)=>Int): (Array[Int], Array[Int], Int)=>Boolean = {

        def canMove(shape: Array[Int], shaft: Array[Int], shaftIndex: Int) = {
            val overlap = 
                shape
                .reverseIterator
                .map(shiftFn)
                .zipWithIndex
                .map(e => e._1 & shaft(shaftIndex - e._2))
                .find(x => x > 0)

            val outOfBounds = shape.iterator
                .map(cmpFn)
                .find(v=> v > 0)
            overlap.isEmpty && outOfBounds.isEmpty
        }
        canMove
    }

    // val canMoveLeft = mkCanMove( _<<1, (_ & 0x40) )
    // val canMoveRight = mkCanMove( _>>1, (_ & 0x1) )

    def canMoveLeft = mkCanMove( _<<1, (_ & 0x40) )
    def canMoveRight = mkCanMove( _>>1, (_ & 0x1) )


    def writeFile(name: String, data: String) = {
        val file = new File(name)
        val fw = new BufferedWriter(new FileWriter(file))
        fw.write(data)
        fw.close()
    }
}