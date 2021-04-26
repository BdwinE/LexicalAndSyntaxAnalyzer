import scala.collection.mutable.ArrayBuffer
/*
*Emeka Edwin Asoluka
 */
class Node(lexeme: String, token:String) {

  var parent: Node = _
  val branches: ArrayBuffer[Node] = new ArrayBuffer[Node]
  var value = lexeme//what's held within the node
  var toke = token

  def addBranch(branch: Node): Unit = branches.addOne(branch)
  def addParent(parent: Node) {
    this.parent = parent
  }
}

