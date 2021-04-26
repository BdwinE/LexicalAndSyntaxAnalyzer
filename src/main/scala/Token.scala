/*Emeka Edwin Asoluka
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Activity 06 - Token
 */

object Token extends Enumeration {
  val EOF           = Value
  val PROGRAM       = Value// 'program'
  val PERIOD        = Value// '.'
  val IDENTIFIER    = Value// letter{letter}
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
  val ARITH_OP      = Value
  val INT_LITERAL   = Value// Digit {Digit}
  val BOOL_EXPR     = Value// ( ´>´ | ´>=´ | ´=´ | ´<=´ | ´<´ )
  val BOOL_LITERAL  = Value// ('true' | 'false')
}
