package day07

import scala.io.Source

object TreeMain {
  def main(args: Array[String]) = {
    val actions = Source.fromResource("day07_input.txt").getLines()
    // val actions = Source.fromResource("day07_sample.txt").getLines()

    val root = new Directory("/")

    populateRoot(root, actions)

    // root.print("", "  ")

    val dirSizes = sumTreeSizes(root, "/")
    dirSizes.foreach(f => println(s"${f._1} : ${f._2}"))
    
    val dirResult = dirSizes
      .filter(d => d._2 <= 100000)
      .map(d => d._2).sum

    println(s"Part 1: sum of directories under 100k bytes: $dirResult")

    val rootSize = dirSizes.get("/")
    val spaceAvailable = 70000000 - rootSize.get
    val spaceNeeded = 30000000 - spaceAvailable

    val smallestSufficientDir = dirSizes
      .filter(d => d._2 >= spaceNeeded)
      .map(d => d._2)
      .min

    println(s"Part 2: Delete the directory with this size: $smallestSufficientDir (should be bigger than $spaceNeeded)")
  }


  def populateRoot(root: Directory, commandStream: Iterator[String]) = {

    var cwd: Option[Directory] = Some(root)

    commandStream.foreach(line => {
      line match {
        case s if s.startsWith("$") => {
          val result = cwd.map(d => processCommand(d, line))
          result match {
            case None => println(s"Unable to process line: $line")
            case Some(ToRoot()) => cwd = Some(root)
            case Some(SetCWD(dir)) => {
              cwd = dir
            }
            case Some(NoAction()) => {}
          }
        }

        // file
        case s if s.matches("^\\d+ .*") => {
          val parts = s.split(" ", 2)
          cwd.map(_.add(File(parts(1), parts(0).toInt)))
        }

        // directory
        case s if s.matches("dir .*") => {
          val name = s.split(" ")(1)
          cwd.map(_.add(new Directory(name)))
        }

      }
    })

  }

  def processCommand(cwd: Directory, line: String): CommandResult = {
    val cd     = "$ cd "
    val cdRoot = "$ cd /"
    val cdUp   = "$ cd .."
    val ls     = "$ ls"

    line match {
        case `cdRoot` => ToRoot()
        case `cdUp`   => SetCWD(cwd.parent)
        case s if s.startsWith(cd) => {
          val name = s.takeRight(s.length - cd.length())
          SetCWD(cwd.dir(name))
        }

        case `ls` => NoAction()
        case _ => NoAction()
      }
  }

  def sumTreeSizes(root: Directory, absPath: String): Map[String, Int] = {
    val fileSizes = root.files().map(_.length).sum

    var dirMap = root.dirs
      .map(entry => sumTreeSizes(entry, s"$absPath${entry.name}/"))
      .flatten
      .toMap
    val dirSizes = root.dirs.map(d => dirMap.get(s"$absPath${d.name}/").get).sum
      
    dirMap + (s"$absPath" -> (fileSizes + dirSizes ))
  }
}

sealed trait CommandResult

case class NoAction() extends CommandResult
case class ToRoot() extends CommandResult
case class SetCWD(newCwd: Option[Directory]) extends CommandResult