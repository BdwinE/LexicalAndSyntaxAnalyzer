import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/*Emeka Edwin Asoluka
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Activity 09 - Grammar
 */

class Grammar(private var source: String) {

  private var productions = new ArrayBuffer[String]
  for (line <- Source.fromFile(source).getLines)
    productions += line

  def getLHS(index: Integer): String = {
    val production = productions(index)
    production.split("->")(0).trim()//.strip()
  }

  def getRHS(index: Integer) = {
    val production = productions(index)
    production.split("->")(1).trim()/*.strip()*/.split(" ")
  }

  override def toString: String = {
    var out = ""
    for (i <- 0 until productions.length)
      out += i + ". " + getLHS(i) + " -> " + getRHS(i).mkString(" ") + "\n"
    out
  }
}

object MMAin{
  def main(args: Array[String]): Unit ={
    var g:Grammar = new Grammar("grammar.txt")
    print(g.getLHS(10))
  }
}