package edu.towson.cosc.cosc455.nhollo1.project1

import scala.collection.mutable.ListBuffer

object CONSTANTS {
  val DOC_BEGIN     : String = 	"\\BEGIN"
  val DOC_END       : String = 	"\\END"
  val TITLE         : String = "\\TITLE["
  val BRACK_END     : String = "]"
  val LINK_BEGIN    : String = "["
  val PARA_BEGIN    : String = "\\PARAB"
  val PARA_END      : String = "\\PARAE"
  val BOLD          : String = "*"
  val PAREN_BEGIN   : String = "("
  val PAREN_END     : String = ")"
  val EQUALS        : String = "="
  val UL            : String = "+"
  val NEW_LINE      : String = "\\\\"
  val HEAD          : String = "#"
  val IMG           : String = "!["
  val DEF           : String = "\\DEF["
  val USE           : String = "\\USE["
  val TEXT          : String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789,.;'-_|"
  val HTH_TEXT      : String = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789,.;'-_|"
  val SPEC          : String = "\\#*[!]+()="

  val Constants : List[String] = List(DOC_BEGIN, DOC_END, TITLE, BRACK_END, HEAD, PARA_BEGIN, PARA_END, BOLD, UL,
    NEW_LINE, LINK_BEGIN, PAREN_BEGIN, PAREN_END, IMG, DEF, EQUALS, USE)


  //declaration of special characters for gittex

  var specialChars: ListBuffer[Char] = new ListBuffer[Char]
  val asterisk: Char  =  '*'
  val hash: Char  =  '#'
  val plus: Char = '+'
  val equal: Char  =  '='
  val backslash: Char  =  '\\'
  val exclamation: Char = '!'
  val leftBracket: Char = '['
  val rightBracket: Char = ']'
  val leftParentheses: Char = '('
  val rightParentheses: Char = ')'

  specialChars += asterisk
  specialChars += hash
  specialChars += plus
  specialChars += equal
  specialChars += backslash
  specialChars += exclamation
  specialChars += leftBracket
  specialChars += rightBracket
  specialChars += leftParentheses
  specialChars += rightParentheses


  // declaring end of line array
  val EOFL: Array[Char] = Array ('\r', '\t', ' ', '\n')

}
