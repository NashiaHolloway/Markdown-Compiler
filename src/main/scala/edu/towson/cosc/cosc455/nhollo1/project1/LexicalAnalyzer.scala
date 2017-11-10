package edu.towson.cosc.cosc455.nhollo1.project1

import scala.collection.mutable.ArrayBuffer //for ArrayBuffer

class LexicalAnalyzer extends LexicalAnalyzerTrait {

  //declare values/variables
  var nextChar: Char = ' '
  var lookupList : List[String] = List()
  var source: String = ""
  var token = new ArrayBuffer[Char](50) //resizable
  var filePos: Int = -1 //file position
  var length: Int = 0

  /**
    * Starter method
    * Don't really need, but makes my life easier
    * @param file1
    */
  def start(file1: String): Unit = {
    initializeLookup()
    source = file1
    getChar()
    getNextToken()
  }

  /**
    * Called from the getNextToken method to grab new character
    */
  override def addChar(): Unit = {
    token += nextChar
    length += 1
  }

  /**
    * Get the next character
    */
  override def getChar(): Unit = {
    if(filePos < source.length - 1){
      filePos += 1
      nextChar = source.charAt(filePos)

    }
    else {

    }
  }

  /**
    * Gets the next token.
    */
  override def getNextToken(): Unit = {

    if(isSpace(nextChar)){
      getNonBlank()
    }
    if(nextChar.equals('[') || nextChar.equals(']') || nextChar.equals('(') || nextChar.equals(')') || nextChar.equals('!')
      || nextChar.equals('#') || nextChar.equals('\\') || nextChar.equals('*') || nextChar.equals('+') || nextChar.equals('=')){
      if(nextChar.equals('\\')){
        addChar()
        getChar()
        while(!isSpace(nextChar) && !nextChar.equals('[')){
          addChar()
          getChar()
        }
        if(nextChar.equals('[')){
          addChar()
          getChar()
        }
      }
      //for Image
      else if(nextChar.equals('!')){
        addChar()
        getChar()
        if(nextChar.equals('[')){
          addChar()
          getChar()
        }
      }
      else {
        addChar()
        getChar()
      }
      wrap()
    }
    else if(!isSpace(nextChar)){
      while(isText(nextChar.toString)){
        addChar()
        getChar()
      }
      Compiler.currentToken = token.mkString
    }
    else {
      println("Can't find next token")
    }
    token.clear()
  }

  /**
    * Packages the new token if lookup() is passed
    */
  def wrap(): Unit = {
    var newT: String = token.mkString
    if(lookup(newT)){
      Compiler.currentToken = newT
      //token.clear()
    }
  }

  /**
    * If the token is legal, return true, else return false
    * @return
    */
  override def lookup(newT: String): Boolean = {
    if(!lookupList.contains(newT)){
      println("LEXICAL ERROR: The lexical token is incorrect, " + newT + " was found.")
      System.exit(1)
      return false
    }
    true
  }

  /**
    * Called in wrap()
    * Used to set currentToken in compiler to next
    * @param currentToken
    */
  def setCurrent(currentToken: String): Unit = {
    Compiler.currentToken = currentToken
  }

  /**
    * Returns true or false if ii is text or not.
    * You can have whitespace in text.
    * @param text
    * @return
    */
  def isText(text: String): Boolean = {
    text match {
      case "\\" => false
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

  /**
    * Method to get tokens that aren't text
    */
  def getNotText(): Unit = {
    while (nextChar.equals(' ') || nextChar.equals('\r') || nextChar.equals('\n') || nextChar.equals('\t')) {
      getChar()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOC_END)) {
        getChar()
        return
      }
    }
  }

  /**
    * Determines if the current character is a space
    * @param c
    * @return
    */
  def isSpace(c: Char): Boolean = {
    if(c == ' ' || c == '\n' || c == '\r' || c == '\b' || c == '\t' || c == '\f'){
      true
    }
    else false
  }

  /**
    * Method to get non-blanks.  Called from start()
    */
  def getNonBlank(): Unit = {
    while(isSpace(nextChar)){
      getChar()
    }
  }

  /**
    * Method called from start() to initialize the list used in getNextToken() to make sure token is valid.
    */
  def initializeLookup(): Unit = {
    lookupList = List("\\BEGIN", "\\begin",
                  "\\END", "\\end",
                  "\\TITLE[", "\\title[",
                  "\\DEF[", "\\def[",
                  "\\USE[", "\\use[",
                  "\\PARAB", "\\parab",
                  "\\PARAE", "\\parae",
                  "]", "#", "*", "+", "\\\\", "\\",
                  "[", "(", ")", "![", "=",
                  )
  }
}
