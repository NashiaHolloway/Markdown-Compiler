package edu.towson.cosc.cosc455.nhollo1.project1

/**
  * Created by Nashia Holloway
  * COSC455 Project 1
  * 11/14/17
  */

class LexicalAnalyzer extends LexicalAnalyzerTrait {

  var nextChar: Char = ' '
  var token: String = ""
  var filePos: Int = 0
  val end: Array[Char] = Array ('\r', '\t', ' ', '\n')
  val importantTokens : List[String] = List(CONSTANTS.DOC_BEGIN, CONSTANTS.DOC_END, CONSTANTS.TITLE, CONSTANTS.BRACK_END,
    CONSTANTS.HEAD, CONSTANTS.PARA_BEGIN, CONSTANTS.PARA_END, CONSTANTS.BOLD, CONSTANTS.UL,
    CONSTANTS.NEW_LINE, CONSTANTS.LINK_BEGIN, CONSTANTS.PAREN_BEGIN, CONSTANTS.PAREN_END, CONSTANTS.IMG, CONSTANTS.DEF,
    CONSTANTS.EQUALS, CONSTANTS.USE)

  /**
    * Method to add char to token string
    */
  override def addChar(): Unit = {
    token += nextChar
  }

  /**
    * Method to get next char, if not at the end of the file.  Increments file position.
    */
  override def getChar(): Unit = {
    if(filePos < Compiler.fileLength){
      nextChar = Compiler.fileContents.charAt(filePos)
      filePos += 1
    }
  }

  /**
    * Method to get next token
    */
  override def getNextToken(): Unit = {
    clearToken() //initialize token string
    getChar()
    getNotText()

    if(nextChar.equals('[') || nextChar.equals(']') || nextChar.equals('(') || nextChar.equals(')') || nextChar.equals('!')
      || nextChar.equals('#') || nextChar.equals('\\') || nextChar.equals('*') || nextChar.equals('+') || nextChar.equals('=')){
      if(nextChar.equals('*')){
        addChar()
        getChar()
      }
      else if(nextChar.equals(')')){
        addChar()
        getChar()
      }
      else if(nextChar.equals('+')){
        addChar()
        token += textEnder() //adds text until end, excluding spaces between words, end of lines, and end of tokens
      }
      else if(nextChar.equals('\\')){ //for \BEGIN, \END, \TITLE[, etc.
        addChar()
        token += textEnder()
        if(nextChar.equals(CONSTANTS.NEW_LINE.charAt(1))){
          addChar()
          Compiler.currentToken = token
          filePos += 1
          return
        }
        if(nextChar.equals('[')){
          addChar()
          Compiler.currentToken = token
          return
        }
        if(nextChar.equals(']')){
          addChar()
        }
        if(CONSTANTS.DOC_END.contains(token.toUpperCase())){
          getNotText()
          if(filePos - Compiler.fileLength != 0){
            filePos = filePos - 1
            getNextToken()
            println("There shouldn't be anything after \\END")
            System.exit(1)
          }
        }
      }
      else if(nextChar.equals('#')){
        addChar()
        token += textEnder()
      }
      else if(nextChar.equals('!')){
        addChar()
        getChar()
        if(nextChar.equals('[')){
          addChar()
          if(lookup()){ //if it passes lookup, set token
            Compiler.currentToken = token
          }
        }
      }
      else if(nextChar.equals('[') || nextChar.equals(']') || nextChar.equals('(') || nextChar.equals(')') || nextChar.equals('!')
        || nextChar.equals('#') || nextChar.equals('\\') || nextChar.equals('*') || nextChar.equals('+') || nextChar.equals('=')){
        addChar()
      }
      //to get rid if \n, \t at the end of token in order for it to pass lookup
      if(token.endsWith("\n") || token.endsWith("\t")){
        token = token.substring(0, token.length - 1)
      }
      if(lookup()){
        Compiler.currentToken = token
        return
      }
      else {
        println("LEXICAL ERROR: The lexical token is incorrect, " + token + " was found.")
        System.exit(1)
      }
    }
    else if(isText(nextChar)){
      addChar()
      token += textEnder()
      if(nextChar.equals(']') || nextChar.toString.equals(CONSTANTS.PARA_END) || nextChar.equals('=')
        || nextChar.equals('\\') || nextChar.equals(')')){
        filePos -= 1
      }
      Compiler.currentToken = token
    }
  }

  /**
    * Returns true if token is in list of important constants, AKA \BEGIN, \END, etc.
    * Returns false if not.
    * @return
    */
  override def lookup(): Boolean = {
    if(importantTokens.contains(token.toUpperCase())){
      true
    }
    else {
      false
    }
  }

  /**
    * Helper method to get rid of lest token in preparation for the next
    */
  def clearToken(): Unit = {
    token = ""
  }

  /**
    * Helper method to get characters while nextChar isn't text
    */
  def getNotText(): Unit = {
    while ((nextChar.equals(' ') || nextChar.equals('\r') || nextChar.equals('\n') || nextChar.equals('\t')) && (filePos < Compiler.fileLength)) {
      getChar()
    }
  }

  /**
    * Reference to Ender's game, except it's not a game.... I'm stressed and have overdosed on RedBull.  I can't feel my fingers.
    */
  def textEnder(): String = {
    var text: String = "" //initialize text string
    getChar()
    while(filePos < Compiler.fileLength && !end.contains(nextChar) && !CONSTANTS.SPEC.contains(nextChar)){
      text += nextChar //add nextChar to text string
      getChar()
    }
    if(nextChar.equals('\n')){
      text += nextChar
    }
    if(nextChar.equals('\r')){
      getChar()
      if(nextChar.equals('\t')){
        text += nextChar
      }
    }
    else {

    }
    return text
  }

  /**
    * Method to return true if text, and false if not
    * @param nextChar
    * @return
    */
  def isText(nextChar: Char): Boolean = {
    nextChar match {
      case '\\' => false
      case '#' => false
      case '!' => false
      case '*' => false
      case '=' => false
      case '+' => false
      case '[' => false
      case ']' => false
      case '(' => false
      case ')' => false
      case '\n' => false
      case '\t' => false
      case '\b' => false
      case ' ' => false
      case _ => true
    }
  }
}
