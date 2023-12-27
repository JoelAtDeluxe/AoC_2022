package day17


object Grid {

    def applyShape(shaft: Array[Int], shape: Array[Int], shaftIndex: Int) = {
        for (i <- (0 until shape.length)) {
            shaft(shaftIndex - i) = shaft(shaftIndex - i) | shape(shape.length-1-i)
        }
    }

    def extend(shaft: Array[Int]): Array[Int] = {
        extension ++ shaft
    }

    def drawShaft(shaft: Array[Int]): String = {
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
        val upperLimit = 2800 // shouldn't need to grow if this accounts for the simulation + 95 lines
        (0 until upperLimit).map(x => if(x==upperLimit-1) 0x7F else 0).toArray
    }

    val extension = (0 until 1000).map(x=>0).toArray

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