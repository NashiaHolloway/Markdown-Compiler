package edu.towson.cosc.cosc455.nhollo1.project1

import scala.collection.mutable.ArrayBuffer //for ArrayBuffer

class LexicalAnalyzer extends LexicalAnalyzerTrait {
  //declare values/variables
  var nextChar: Char = ' '
  var file: Array[Char] = Array()
  val lookupArr = new Array[String](20) //not resizeable
  var token = new ArrayBuffer[Char](50) //resizable
  var fileSize: Int = 0
  var filePos: Int = -1 //file position



  def start(File: String): Unit = {
    initializeLookupArray()
    file = File.toCharArray
    fileSize = file.length - 1
  }

  /*
    Called from the getNextToken method to grab new character
   */
  override def addChar(): Unit = {
    token += nextChar
  }

  /*
    Get the next character
   */
  override def getChar(): Unit = {
    if(filePos < fileSize){
      filePos += 1
      nextChar = file.charAt(filePos)
    }
    else {

    }
  }

  /*
    Gets next token
   */
  override def getNextToken(): Unit = {
    getChar()
    getNotText()

    //Break into lexical units
    if(nextChar.equals('+') || nextChar.equals('=') || nextChar.equals('+') || nextChar.equals('+') ||
      nextChar.equals('+') || nextChar.equals('+') || nextChar.equals('+')){
      addChar()
    }
    else if(nextChar.equals('\\')){
      addChar()
      getChar()
      while(!nextChar.equals('[') && nextChar != '\n' && nextChar != '\\'){
        if(nextChar.equals('\r')){
          addChar()
        }
        else {
          addChar()
          getChar()
        }
      }
      if(nextChar.equals('[') || nextChar.equals('\\')){
        addChar()
      }
    }
    else if(nextChar.equals('*')){
      addChar()
      getChar()
      if(nextChar.equals('*')){
        addChar()
        getChar()
        wrap()
      }
      else {
        filePos -= 1
      }
    }
    else if(nextChar.equals('!')){
      addChar()
      getChar()
      if(nextChar.equals('[')){
        addChar()
      }
    }
    else {
      addChar()
      getChar()
      while(!CONSTANTS.SPEC.contains(nextChar)){
        addChar()
        if(filePos < fileSize) {
          getChar()
        }
        else {

        }
      }
      filePos -= 1
    }
    wrap()
  }

  /*
    Not really sure what I can use this for...
   */
  override def lookup(): Boolean = ???

  /*
    For when the string successfully passed lookup
   */
  def wrap(): Unit = {
    val passToken: String = token.mkString
    if(lookupArr.contains(passToken.toUpperCase)) {
      setCurrent(passToken) //token to compiler
      token.clear()
    }
    else if(isText(passToken)){
      setCurrent(passToken) //token to compiler
      token.clear()
    }
    else {
      println("Lexical Error: The lexical token is incorrect, " + passToken + " was found.")
      System.exit(1)
    }
  }

  /*
    Called in wrap()
    Used to set currentToken in compiler to next
   */
  def setCurrent(currentToken: String): Unit = {
    Compiler.currentToken = currentToken
  }

  /*
    returns true or false if it is text or not
  */
  def isText(text: String): Boolean = {
    text match {
      case "\\\\" => false
      case "#" => false
      case "!" => false
      case "*" => false
      case "=" => false
      case "+" => false
      case "[" => false
      case "]" => false
      case "(" => false
      case ")" => false
      case _ => true
    }
  }

  /*
    Method to get tokens that aren't text
   */
  def getNotText(): Unit = {
    nextChar match {
      case ' ' => getChar()
      case '\r' => getChar()
      case '\n' => getChar()
      case '\t' => getChar()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOC_END)){
        getChar()
        return
      }
    }
  }

  /*
    Determines if the current character is a space
   */
  def isSpace(c: Char): Boolean = {
    if(c == ' '){
      true
    }
    else false
  }

  def getNonBlank(): Unit = {
    while(isSpace(nextChar)){
      getChar()
    }
  }

  def initializeLookupArray(): Unit = {
    lookupArr(0) = "\\BEGIN"
    lookupArr(1) = "\\END"
    lookupArr(2) = "\\TITLE["
    lookupArr(3) = "]"
    lookupArr(4) = "#"
    lookupArr(5) = "\\PARAB"
    lookupArr(6) = "\\PARAE"
    lookupArr(7) = "\\DEF["
    lookupArr(8) = "\\USE["
    lookupArr(9) = "**"
    lookupArr(10) = "*"
    lookupArr(11) = "+"
    lookupArr(12) = "\\\\"
    lookupArr(13) = "["
    lookupArr(14) = "("
    lookupArr(15) = ")"
    lookupArr(16) = "="
    lookupArr(17) = "!["
    lookupArr(18) = "]"
    lookupArr(19) = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toString()
  }
}
