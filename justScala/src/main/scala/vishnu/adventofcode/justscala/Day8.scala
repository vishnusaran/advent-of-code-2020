package vishnu.adventofcode.justscala

import scala.collection.mutable

object Day8 {
  val inputRegex = "(nop|acc|jmp) ([\\+-]\\d+)".r

  def parse(str: String) = {
    val inputRegex(inst, arg) = str
    (inst, arg.toInt)
  }

  def executeInstruction(input: Seq[(String, Int)]): Int = {
    var pos = 0
    var acc = 0
    val bitSet = new mutable.BitSet(input.length)
    while (pos < input.length && !bitSet(pos)) {
      bitSet += pos
      val (ins, arg) = input(pos)
      ins match {
        case "acc" =>
          acc += arg
          pos += 1
        case "jmp" => pos += arg
        case _ => pos += 1
      }
    }
    if (pos == input.length) println("End reacched") else println(s"Loop found.")
    acc
  }

  def getLoopStart(input: Seq[(String, Int)]): Option[Int] = {
    var pos = 0
    val bitSet = new mutable.BitSet(input.length)
    var maxPosReaded = -1
    while (pos < input.length && !bitSet.contains(pos)) {
      bitSet += pos
      maxPosReaded = Math.max(pos,maxPosReaded)
      val (ins, arg) = input(pos)
      ins match {
        case "jmp" =>
          pos += arg
        case _ => pos += 1
      }
    }
    if (pos != input.length) Some(maxPosReaded) else None
  }

  def fixLoop(input: Seq[(String, Int)]) = {
    val inputArr = input.toArray
    val loopTrigger = getLoopStart(inputArr).get

    var i = loopTrigger
    var stop = false

    do {
      while (!(inputArr(i)._1 == "nop" || inputArr(i)._1 == "jmp")) {
        i -= 1
      }
      swap(inputArr, i)

      val loopFound = getLoopStart(inputArr)
      println(s"Checking $i : $loopFound")
      if (loopFound.isDefined) {
        swap(inputArr, i)
        i -= 1
      } else {
        println("No lOop found")
        stop = true
      }
    } while (!stop && i > 0)

    inputArr.toSeq

  }

  private def swap(input: Array[(String, Int)], pos: Int) = {
    val newInst = if (input(pos)._1 == "jmp") "nop" else "jmp"
    input(pos) = newInst -> input(pos)._2
  }

  def main(args: Array[String]): Unit = {
    //    val stream = this.getClass.getResourceAsStream("/day8_sample.txt")
    val stream = this.getClass.getResourceAsStream("/day8_input.txt")
    val arrayInput: Array[(String, Int)] = scala.io.Source.fromInputStream(stream).getLines().map(parse).toArray
//    println(executeInstruction(arrayInput))
    val newInput = fixLoop(arrayInput)
    println(executeInstruction(newInput))

  }
}
