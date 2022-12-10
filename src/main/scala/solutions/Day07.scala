package solutions

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable
import solutions.Day

class Day07 extends Day:
  val day = 1;
  val testInput = """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"""

  val sizeThreshold = 100000

  case class File(name: String, size: Int, parent: Option[File]):
    var children = ArrayBuffer[File]()

    def path: String =
      if (parent.isDefined) {
        return parent.get.path + "/" + name
      }
      return name

    def isDir: Boolean = children.length > 0

    def computeSize: Int =
      if (size == 0) {
        return children.map(f => f.computeSize).sum
      }
      return size

    override def toString: String =
      s"($name: $size)"

  def parseInput(input: List[String]): mutable.LinkedHashMap[String, File] =
    var dirs = mutable.LinkedHashMap[String, File]()
    var current: Option[File] = None

    for (line <- input) {
      if (line.startsWith("$ cd")) {
        val folderName = line.slice("$ cd ".length(), line.length())
        if (folderName == "..") {
          current = current.get.parent
        } else {
          val candidate = File(folderName, 0, current)
          if (dirs.contains(candidate.path)) {
            current = dirs.get(candidate.path)
          } else {
            if (current.isDefined) {
              current.get.children += candidate
            }
            current = Option(candidate)
            dirs.put(candidate.path, candidate)
          }
        }
      } else if (line.startsWith("$ ls")) {
        // do nothing
      } else {
        val pair = line.split(" ")
        val size = pair(0).toIntOption
        if (size.isDefined) {
          // it's a file
          val file = File(pair(1), size.get, current)
          current.get.children += file
        } else if (pair(0) == "dir") {
          // it's a directory
          val file = File(pair(1), 0, current)
          current.get.children += file
          dirs.put(file.path, file)
        } else {
          println(s"unknown!! <$line>")
        }
      }
    }
    return dirs

  def solveA(input: List[String]): String =
    val dirs = parseInput(input)

    val root = dirs.get("/").get
    val sizes = mutable.LinkedHashMap[String, Int]()
    computeSizes(root, sizes)

    return sizes
      .filter((path, size) => size < sizeThreshold)
      .map((path, size) => size)
      .sum
      .toString

  def computeSizes(
      root: File,
      sizes: mutable.LinkedHashMap[String, Int],
  ): Int =
    if (sizes.contains(root.path)) {
      return sizes.get(root.path).get
    }
    val size =
      if (root.isDir) root.children.map(f => computeSizes(f, sizes)).sum
      else root.size
    if (root.isDir) sizes.put(root.path, size)
    return size

  val totalSize = 70000000
  val requiredSpace = 30000000
  def solveB(input: List[String]): String =
    val dirs = parseInput(input)

    val root = dirs.get("/").get
    val sizes = mutable.LinkedHashMap[String, Int]()
    computeSizes(root, sizes)

    val usedSpace = totalSize - sizes.get("/").get
    val amountToDelete = requiredSpace - usedSpace

    val sorted = sizes
      .map((path, size) => size)
      .filter(size => size > amountToDelete)
      .toList
      .sorted
    return sorted(0).toString()
