import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/*Emeka Edwin Asoluka
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Activity 06 - Lexical Analyzer
 */

/*
program = ´program´ identifier body ´.´
identifier = letter { ( letter | digit ) }
body = [ var_sct ] block
var_sct = ´var´ var_dcl { ´;´ var_dcl }
var_dcl = identifier { identifier } ´:´ type
type = ´Integer´ | ´Boolean´
block = ´begin´ stmt { ´;´ stmt } ´end´
stmt = assgm_stmt | read_stmt | write_stmt | if_stmt | while_stmt | block
assgm_stmt = identifier ´:=´ expr
read_stmt = ´read´ identifier
write_stmt = ´write´ ( identifier | literal )
if_stmt = ´if´ bool_expr ´then´ stmt [ ´else´ stmt ]
while_stmt = ´while´ bool_expr ´do´ stmt
expr = arithm_expr | bool_expr
arithm_expr = arithm_expr ( ´+´ | ´-´ ) term | term
term = term ´*´ factor | factor
factor = identifier | int_literal
literal = int_literal | bool_literal
int_literal = digit { digit }
bool_litreal = ´true´ | ´false´
bool_expr = bool_literal | arithm_expr ( ´>´ | ´>=´ | ´=´ | ´<=´ | ´<´ ) arithm_expr
letter = ´a´ | ´b´ | ´c´ | ´d´ | ´e´ | ´f´ | ´g´ | ´h´ | ´i´ | ´j´ | ´k´ | ´l´ | ´m´ | ´n´ | ´o´ | ´p´ | ´q´ | ´r´ | ´s´ | ´t´ | ´u´ | ´v´ | ´w´ | ´x´ | ´y´ | ´z´ | ´A´ | ´B´ | ´C´ | ´D´ | ´E´ | ´F´ | ´G´ | ´H´ | ´I´ | ´J´ | ´K´ | ´L´ | ´M´ | ´N´ | ´O´ | ´P´ | ´Q´ | ´R´ | ´S´ | ´T´ | ´U´ | ´V´ | ´W´ | ´X´ | ´Y´ | ´Z´
digit = ´0´ | ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´
 */

class LexicalAnalyzer(private var source: String) extends Iterable[LexemeUnit] {

  private var input = ""
  private val file = Source.fromFile(source)

  //get input from source file, passed in by the class param
  for (line <- file.getLines())
    input += line + "\n"
  file.close()//close file

  // determines the class of a given character
  private def getCharClass(c: Char): CharClass.Value = {
    if (LexicalAnalyzer.LETTER.contains(c))
      CharClass.LETTER
    else if (LexicalAnalyzer.BLANK.contains(c))
      CharClass.BLANK
    else if (LexicalAnalyzer.BOOL_EXPR.contains(c))
      CharClass.BOOL_EXPR
    else if (LexicalAnalyzer.DIGIT.contains(c))
      CharClass.DIGIT
    else
      CharClass.OTHER
  }

  // reads the input until a non-blank character is found, returning the input updated
  private def readBlanks(): Unit = {
    var foundNonBlank = false
    while (input.length > 0 && !foundNonBlank) {
      val c = input(0)
      if (getCharClass(c) == CharClass.BLANK)
        input = input.substring(1)
      else
        foundNonBlank = true
    }
  }

  override def toString(): String ={
    var string = ""
    val lex = new LexicalAnalyzer(source)
    val it = lex.iterator
    while (it.hasNext) {
      val lexemeUnit = it.next()
      //println(lexemeUnit)
      string += lexemeUnit+"\n"
    }
    string+="(EOF,EOF)"
    string
  }
  def arrayResult(): ArrayBuffer[String] ={
    var aB:ArrayBuffer[String] =  new ArrayBuffer[String]
    var split = toString().split("\n")
    for(i <- 0 to split.length-1){
      aB.append(split(i))
    }
    aB
  }


  def iterator: Iterator[LexemeUnit] = {
    new Iterator[LexemeUnit] {

      override def hasNext: Boolean = {
        readBlanks()
        input.length > 0
      }

      override def next(): LexemeUnit = {
        if (!hasNext)
          new LexemeUnit("", Token.EOF)
        else {
          var lexeme = ""
          readBlanks()
          if (input.length == 0)
            new LexemeUnit(lexeme, Token.EOF)
          else {
            var c = input(0)
            var charClass = getCharClass(c)



            /*val EOF           = Value
            val PROGRAM       = Value// 'program'
            val PERIOD        = Value// '.'
            val IDENTIFIER    = Value// letter{(letter | DIGIT)}
            val VAR           = Value// 'var'
            val SEMICOLON     = Value// ';'
            val COLON         = Value// ':'
            val INTEGER       = Value// 'Integer'
            val BOOLEAN       = Value// 'Boolean'
            val BEGIN         = Value// 'begin'
            val END           = Value// 'end'
            val DEFINITION    = Value// ':='
            val READ          = Value// 'read'
            val WRITE         = Value// 'write'
            val IF            = Value// 'if'
            val THEN          = Value// 'then'
            val ELSE          = Value// 'else'
            val WHILE         = Value// 'while'
            val DO            = Value// 'do'
            val PLUS          = Value// 'plus'
            val MINUS         = Value// 'minus'
            val TIMES         = Value// '*'
            val INT_LITERAL   = Value// Digit {Digit}
            val BOOL_EXPR     = Value// ( ´>´ | ´>=´ | ´=´ | ´<=´ | ´<´ )
            val BOOL_LITERAL  = Value// ('true' | 'false')
            */
            var lexeme = ""

            //System.out.println(charClass)
            if(charClass==CharClass.LETTER){
              //read until find blank
              while((charClass==CharClass.LETTER || charClass==CharClass.DIGIT) && input.length>0){
                c = input(0)
                lexeme += c
                input = input.substring(1)
                c = input(0)//new input value
                charClass =  getCharClass(c)//new char class
              }
              c='\n'//to avoid error in code
            }


            //TODO: recognize 'program' as a PROGRAM
            if(lexeme.equals("program")){
              //System.out.println("in program")
              return new LexemeUnit(lexeme, Token.PROGRAM)
            }
            //TODO: recognize '.' as a PERIOD
            if(c=='.') {
              lexeme+=c
              input= input.substring(1)
              return new LexemeUnit(lexeme, Token.PERIOD)
            }
            //TODO: recognize 'var' as a VAR
            if(lexeme.equals("var")){
              return new LexemeUnit(lexeme, Token.VAR)
            }
            //TODO: recognize ';' as a SEMICOLON
            if(c==';') {
              lexeme+=c
              input= input.substring(1)
              return new LexemeUnit(lexeme, Token.SEMICOLON)
            }
            //TODO: recognize ':' as a COLON or ":=" as a DEFINITION
            if(c==':') {
              if (input.length > 1 && input.charAt(1) == '=') {
                lexeme += c
                input = input.substring(1)
                c = input(0)
                lexeme += c
                input = input.substring(1)
                return new LexemeUnit(lexeme, Token.DEFINITION)
              }
              else {
                lexeme += c
                input = input.substring(1)
                return new LexemeUnit(lexeme, Token.COLON)
              }
            }
            //TODO: recognize 'Integer' as a INTEGER
            if(lexeme.equals("Integer")){
              return new LexemeUnit(lexeme, Token.INTEGER)
            }
            //TODO: recognize 'Boolean' as a BOOLEAN
            if(lexeme.equals("Boolean")){
              return new LexemeUnit(lexeme, Token.BOOLEAN)
            }
            //TODO: recognize 'begin' as a BEGIN
            if(lexeme.equals("begin")){
              return new LexemeUnit(lexeme, Token.BEGIN)
            }
            //TODO: recognize 'end' as a END
            if(lexeme.equals("end")){
              return new LexemeUnit(lexeme, Token.END)
            }
            //TODO: recognize 'read' as a READ
            if(lexeme.equals("read")){
              return new LexemeUnit(lexeme, Token.READ)
            }
            //TODO: recognize 'write' as a WRITE
            if(lexeme.equals("write")){
              return new LexemeUnit(lexeme, Token.WRITE)
            }
            //TODO: recognize 'if' as a IF
            if(lexeme.equals("if")){
              return new LexemeUnit(lexeme, Token.IF)
            }
            //TODO: recognize 'then' as a THEN
            if(lexeme.equals("then")){
              return new LexemeUnit(lexeme, Token.THEN)
            }
            //TODO: recognize 'else' as a ELSE
            if(lexeme.equals("else")){
              return new LexemeUnit(lexeme, Token.ELSE)
            }
            //TODO: recognize 'while' as a WHILE
            if(lexeme.equals("while")){
              return new LexemeUnit(lexeme, Token.WHILE)
            }
            //TODO: recognize 'do' as a DO
            if(lexeme.equals("do")){
              return new LexemeUnit(lexeme, Token.DO)
            }
            //TODO: recognize '+', '-', and '*' as arithmetic operator
            if(c=='*' || c=='+' || c=='-') {
              lexeme+=c
              input= input.substring(1)
              return new LexemeUnit(lexeme, Token.ARITH_OP)
            }
            //TODO: recognize a digit followed by 0 or more digits as a INT_LITERAL
            if(charClass==CharClass.DIGIT){
              //read until find blank or non digit
              //System.out.println("here")
              while(charClass==CharClass.DIGIT && input.length>0){
                //c = input(0)
                lexeme += c
                input = input.substring(1)
                c = input(0)//new input value
                charClass =  getCharClass(c)//new char class
              }
              return new LexemeUnit(lexeme, Token.INT_LITERAL)
            }
            //TODO: recognize ( ´>´ | ´>=´ | ´=´ | ´<=´ | ´<´ ) as a BOOL_EXPR
            if(c=='>' || c=='=' || c=='<'){
              //System.out.println("Enter")
              lexeme += c
              if((c=='>' || c=='<') && input.length>=2) {
                if(input.charAt(1)=='=') {
                  //System.out.println("here")
                  input= input.substring(1)
                  c = input(0)
                  lexeme+=c
                }
              }
              input= input.substring(1)
              return new LexemeUnit(lexeme, Token.BOOL_EXPR)
            }
            //TODO: recognize ('true' | 'false') as a BOOL_LITERAL
            if(lexeme.equals("true") || lexeme.equals("false")){
              return new LexemeUnit(lexeme, Token.BOOL_LITERAL)
            }
            //TODO: recognize a letter followed by letters or digit as an IDENTIFIER
            if(lexeme.length>0 && getCharClass(lexeme.charAt(0))==CharClass.LETTER)
              return new LexemeUnit(lexeme, Token.IDENTIFIER)

            // throw an exception if an unrecognizable symbol is found
            System.out.println("Error, " + c + " or " + lexeme + " was unrecognized by the Lexical Analyzer")
            throw new Exception("Lexical Analyzer Error: unrecognizable symbol found!")
          }
        }
      } // end next
    } // end 'new' iterator
  } // end iterator method
} // end LexicalAnalyzer class

object LexicalAnalyzer {
  val LETTER = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val BLANK  = " \n\t"
  val BOOL_EXPR = "><="
  val DIGIT     = "0123456789"

  /*def main(args: Array[String]): Unit = {
    // check if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    val lex = new LexicalAnalyzer(args(0))
    val it = lex.iterator
    while (it.hasNext) {
      val lexemeUnit = it.next()
      println(lexemeUnit)
    }
  } // end main method*/
} // end LexicalAnalyzer object