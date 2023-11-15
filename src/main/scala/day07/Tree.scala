package day07

sealed trait FileSystem {
  def name(): String
}

final case class File(name: String, length: Int) extends FileSystem {
  override def toString() = {
    s"$name (file, size=$length)"
  }
}

class Directory(val name: String) extends FileSystem {
  private var _dirs: Map[String, Directory] = Map()
  private var _files: List[File]            = List()
  private var _parent: Option[Directory]    = None

  def this(name: String, parent: Directory) = {
    this(name)
    this.setParent(parent)
  }

  protected def setParent(d: Directory) = {
    _parent = Some(d)
    this
  }

  def parent(): Option[Directory] = {
    _parent
  }

  def add(items: List[FileSystem]) = {
    _files = items.collect({ case f: File => f }) ++ _files
    val newDirs: Map[String, Directory] = items.collect({ case d: Directory => d })
      .map(_.setParent(this))
      .map(d => (d.name, d))
      .toMap
    _dirs = _dirs ++ newDirs
    this
  }

  def add(item: FileSystem) = {
    item match {
      case f: File => { _files = f +: _files }
      case d: Directory => {
        d.setParent(this)
        _dirs = _dirs + (d.name -> d)
      }
    }
    this
  }

  def dir(name: String): Option[Directory] = {
    _dirs.get(name)
  }

  def dirs(): List[Directory] = {
    _dirs.values.toList
  }

  def print(offset: String = "", growOffset: String=""): Unit = {
    println(s"$offset- ${name} (dir)")
    val suboffset = s"${growOffset}${offset}"
    _dirs.toList.sortBy(entry => entry._1).foreach( entry => {
      entry._2.print(suboffset, growOffset)
    })
    _files.sortBy(f => f.name).foreach(f => println(s"$suboffset- $f"))
  }

  def files(): List[File] = {
    _files
  }
}

// val root = new Directory("/")
//   .add(List(
//     new Directory("a")
//       .add(List(
//         File("a_A", 165),
//         File("a_B", 166),
//         new Directory("zz")
//           .add(List(
//             File("zz_A", 2665)
//           ))
//       )),
//     new Directory("b")
//       .add(List(
//         File("b_A", 265),
//         File("b_B", 266)
//       )),
//     File("rootFile", 1024)
//   ))
