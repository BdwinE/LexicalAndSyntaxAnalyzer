import scala.io.Source
import scala.collection.mutable.ArrayBuffer


/* Emeka Edwin Asoluka
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Activity 09 - SLR Table
 */

class SLRTable(private var source: String) {

  private val actions: scala.collection.mutable.Map[(Int, String), String] =  scala.collection.mutable.Map()
  private val gotos: scala.collection.mutable.Map[(Int, String), String] =  scala.collection.mutable.Map()

  val input = Source.fromFile(source).getLines()
  val inputPointer = Source.fromFile(source).getLines()//for error handingling
  val header = input.next().split(",")
  val eof = header.indexOf("$")
  val tokens = for (i <- 1 to eof) yield header(i)//.toInt
  val variables = for (i <- eof + 1 until header.length) yield header(i)
  while (input.hasNext) {
    val line = input.next() + " "
    val row = line.split(",")
    val state = row(0).toInt
    for (i <- 0 until tokens.length) {
      val token = tokens(i)
      val key = (state, token)
      val value = row(i + 1)
      actions(key) = value
    }
    for (i <- 0 until variables.length) {
      val variable = variables(i)
      val key = (state, variable)
      val value = row(tokens.length + i + 1)
      gotos(key) = value
    }
  }

  def getAction(state: Int, token: String/*Int*/) = actions((state, token))

  def getGoto(state: Int, variable: String) = gotos((state, variable))

  def getRow(state:Integer): Array[String] ={
    var rowArray = new ArrayBuffer[String]
    for(i <- 0 to state){
      inputPointer.next()
    }
    var inSplit = inputPointer.next().split(",")
    inSplit
  }

  override def toString: String = {
    var out = "actions:\n"
    for ((key, value) <- actions)
      out += key + " -> " + value + "\n"
    out += "gotos:\n"
    for ((key, value) <- gotos)
      out += key + " -> " + value + "\n"
    out
  }

}





