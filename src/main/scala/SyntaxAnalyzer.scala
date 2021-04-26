import scala.collection.mutable.ArrayBuffer
/*
*Emeka Edwin Asoluka
 */
class SyntaxAnalyzer {

  var stack:ArrayBuffer[String] = new ArrayBuffer[String]
  var trees: ArrayBuffer[Node] = new ArrayBuffer[Node]//trees/ nodes produces during parsing
  var input:ArrayBuffer[String] = _
  var grammar: Grammar = new Grammar("grammar.txt")
  var SLRTable = new SLRTable("slr_table.csv")

  def parse(fileName:String) {
    //print(input)
    input = new LexicalAnalyzer(fileName).arrayResult()
    stack.append("0")//append stack with the first state
    var state = getState(stack)
    var action = ""
    var currentInput:String = ""

    while(input.length>0 && !action.equals("acc")){
      currentInput = input(0)
      action = evaluate(state, currentInput)// also builds tree
      state = getState(stack)//get newest state
    }
    printTree()
  }

  def evaluate(state:Integer, cInput: String) : String = /*ArrayBuffer[String]*/ { //returns action
    //input format (lexeme, token)
    var sCInput = cInput.split(',')
    var lexeme = sCInput(0).substring(1)
    var token = sCInput(1).substring(0, sCInput(1).length - 1)
    var action = ""
    if (token.equals("EOF")) {
      action = SLRTable.getAction(state, "$")
    }
    else if (token.equals("IDENTIFIER")) {
      action = SLRTable.getAction(state, "Identifier") //get the table content for I
    }
    else if (token.equals("INT_LITERAL")) {
      action = SLRTable.getAction(state, "Int_Literal") //get table content for D
    }
    else {
      action = SLRTable.getAction(state, lexeme)
    }

    try {
      if (action.charAt(0) == 's') { //shift
        shift(action.substring(1).toInt, lexeme, token)
      }
      else if (action.charAt(0) == 'r') { //reduce
        reduce(action.substring(1).toInt, lexeme)
      }
    }
    catch{//print expected inputs when invalid input is given
      case e: Exception => println("Syntax Analyzer Error: " +expectedInput(state) + "expected")
      throw new Exception("Enter Valid Input")
    }
    action
    //new ArrayBuffer[String]
  }
  def shift(state:Integer, lexeme:String, token:String): Unit ={
    //push lexeme onto the stack
    push(lexeme)
    //remove lexeme from input
    input.remove(0)//first index, where current lexeme should be
    //push state onto stack
    push(state+"")
    //add new lexeme to tree
    trees.append(new Node(lexeme, token))
  }
  def reduce(productionLine:Integer, lexeme:String): Unit ={
    //create new Node for the reduction
    var reducVariable = grammar.getLHS(productionLine).trim()
    var newNode: Node = new Node(reducVariable, lexeme)
    //figure out how many variables need to be reduced
    var rhs = grammar.getRHS(productionLine)
    //handle case where rhs is epsilon
    var reduceAmount:Integer = 0
    if(rhs(0).equals("''")) {
      reduceAmount = 0
    }
    else{
      reduceAmount = rhs.length
    }

    //make x amount of nodes in the trees branches to newNode. x = reduceAmount
    for(i <- trees.length-reduceAmount to trees.length-1){
      newNode.addBranch(trees(i))
    }
    //remove the nodes that were just added to the newNode from trees
    for(i <- trees.length-reduceAmount to trees.length-1){
      trees.remove(trees.length-1)
    }
    //add newNode to trees
    trees.append(newNode)

    //remove reduceAmount*2 elements from stack
    for(i <- 1 to reduceAmount*2){
      pop//remove last element from stack
    }
    //get the next state
    var state = peak.toInt
    //add the reduceVariable to the stack
    push(reducVariable)
    //get and add the new state to the stack
    push(SLRTable.getGoto(state,reducVariable))
  }
  def printTree(): Unit ={
    var parent = trees(0)//should be first element in tree
    printTreeHelper(parent, 0);
  }
  private def printTreeHelper(current:Node, layerNum:Integer): Unit ={
    if(current==null)
      return
    var tab = ""
    var nextLayerNum = layerNum
    for (t <- 0 to layerNum) {
      tab += "  "
    }
    if(current.value.charAt(0)=='R') {
      //dont print or incrmenet layerNum
    }
    else {
      if(current.toke.equals("IDENTIFIER"))
        println(tab + "Identifier: '" + current.value + "'")
      else if(current.toke.equals("INT_LITERAL"))
        println(tab+ "Int_Literal: '"+ current.value + "'")
      else
        println(tab + current.value)
      nextLayerNum += 1
    }
    for(i <- 0 to current.branches.length-1){
      printTreeHelper(current.branches(i), nextLayerNum)
    }
  }
  def getState(stack:ArrayBuffer[String]): Integer ={
    stack.last.trim().toInt//returns last value on top of stack
  }
  def peak: String ={
    return stack(stack.length-1)
  }
  def pop: Unit ={
    stack.remove(stack.length-1)//remove last value from stack
  }
  def push(str:String): Unit ={
    stack.append(str)
  }

  //for error checking
  def expectedInput(state:Integer):String={
    var expect = ""//a list of the expected chars
    var sRow = SLRTable.getRow(state)
    for(i <- 0 to sRow.length-1){
      if(!sRow(i).equals("") && i!=0 && i<= SLRTable.header.indexOf("$"))
        expect += SLRTable.header(i) + " or "
    }
    expect = expect.substring(0, expect.length-3)//for 3 for the length of the substring " or "
    return expect
  }
}

object Main{
  var sA :SyntaxAnalyzer = new SyntaxAnalyzer;
  def main(args: Array[String]){
    if(args.length<=0)
      println("Input File missing")
    else
      sA.parse(args(0))
  }
}
