package edu.towson.cosc.cosc455.nhollo1.project1

/*
  TO DO: figure out what to do with epsilon.
         figure out how to handle when something is optional, like bold or UL within paragraphs
 */

import scala.collection.mutable.Stack //to add to stack

class SyntaxAnalyzer extends SyntaxAnalyzerTrait {

  var parse = Stack[String]() //to add to stack "parse.push()"

  //For errors and helper methods
  var errorFound : Boolean = false
  def setError() = errorFound = true
  def resetError() = errorFound = false
  def getError() : Boolean = errorFound

  /*
    <gittex>    ::= DOC_BEGIN <var_def> <title> <body> DOC_END
   */
  override def gittex(): Unit = {
    doc_begin()
    while(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEF)){
      var_def()
    }
    title()
    body()
    doc_end()
  }

  /*
    OPTIONAL
    <body>      ::= <inner> <body>
                | <para> <body>
                | <new_line> <body>
                | ε
   */
  override def body(): Unit = {
    //first "if" is to make it optional
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEF) ||
       Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEAD) ||
       Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD_BEGIN) ||
       Compiler.currentToken.equalsIgnoreCase(CONSTANTS.UL) ||
       Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACK_BEGIN) ||
       Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMG) ||
       Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TEXT))
    {
      inner()
      body()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARA_BEGIN)){
      para()
      body()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEW_LINE)){
      new_line()
      body()
    }
  }

  /*
    NOT OPTIONAL
    <title>     ::= TITLE BRACK_BEGIN HTH_TEXT BRACK_END
   */
  override def title(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLE)) {
      parse.push(CONSTANTS.TITLE)
      Compiler.Scanner.getNextToken()
    }
    else {
      print("SYNTAX ERROR: \\TITLE was expected when '" + Compiler.currentToken + "' was found." )
      setError()
    }
    brack_begin()
    hth_text()
    brack_end()
  }

  /*
    OPTIONAL
    <head>      ::= HEAD HTH_TEXT | ε
   */
  override def head(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEAD)){
      parse.push(CONSTANTS.HEAD)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: # was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
    hth_text()
  }

  /*
    <para>      ::= PARB <var_def> <none_or_more> PARE
   */
  override def para(): Unit = {
    para_begin()
    while(!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARA_END)){
      var_def() //if used, it must be the first
      none_or_more()
    }
    para_end()
  }

  /*
    OPTIONAL
    <none_or_more>   ::= <var_use> <none_or_more>
                      | <bold> <none_or_more>
                      | <link> <none_or_more>
                      | HTH_TEXT <none_or_more>
                      | ε
   */
  override def none_or_more(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USE)){
      var_use()
      none_or_more()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD_BEGIN)){
      bold()
      none_or_more()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACK_BEGIN)){
      link()
      none_or_more()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HTH_TEXT)){
      text()
      none_or_more()
    }
    else {
      getError() //no error, since it's optional, just leave the method
    }
  }

  /*
    OPTIONAL
    <inner>    ::= <var_use> <inner>
              | <head> <inner>
              | <bold> <inner>
              | <ul> <inner>
              | <link> <inner>
              | <img> <inner>
              | TEXT <inner>
              | ε
   */
  override def inner(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USE)){
      var_use()
      inner()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEAD)){
      head()
      inner()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD_BEGIN)){
      bold()
      inner()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.UL)){
      ul()
      inner()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACK_BEGIN)){
      link()
      inner()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMG)){
      img()
      inner()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TEXT)){
      text()
      inner()
    }
    else {
      getError() //no error since it's optional
    }
  }

  /*
    OPTIONAL
    <var_def>   ::= DEF BRACK_BEGIN HTH_TEXT EQUALS HTH_TEXT BRACK_END <var_def> | ε
   */
  override def var_def(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEF)){
      parse.push(CONSTANTS.DEF)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: \\DEF was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
    brack_begin()
    hth_text()
    equals()
    hth_text()
    brack_end()
  }

  /*
    OPTIONAL
    <var_use>   ::= USE BRACK_BEGIN HTH_TEXT BRACK_END | ε
   */
  override def var_use(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USE)){
      parse.push(CONSTANTS.USE)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: \\USE was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
    brack_begin()
    text()
    brack_end()
  }

  /*
    OPTIONAL
    <bold>      ::= BOLD_BEGIN TEXT BOLD_END | ε
   */
  override def bold(): Unit = {
    bold_begin()
    hth_text()
    bold_end()
  }

  /*
    OPTIONAL
    <ul>        ::= UL <none_or_more> <ul> | ε
   */
  override def ul(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.UL)){
      parse.push(CONSTANTS.UL)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: + was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
      none_or_more()
      ul()
  }

  /*
    OPTIONAL
    <link>      ::= BRACK_BEGIN HTH_TEXT BRACK_END PAREN_BEGIN HTH_TEXT PAREN_END | ε
   */
  override def link(): Unit = {
    brack_begin()
    hth_text()
    brack_end()
    paren_begin()
    hth_text()
    paren_end()
  }

  /*
    <img>       ::= IMG BRACK_BEGIN HTH_TEXT BRACK_END PAREN_BEGIN HTH_TEXT PAREN_END | ε
   */
  override def img(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMG)){
      parse.push(CONSTANTS.IMG)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: ! was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
    brack_begin()
    hth_text()
    brack_end()
    paren_begin()
    hth_text()
    paren_end()
  }

  /*
    <new_line>  ::= NEW_LINE | ε
   */
  override def new_line(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEW_LINE)){
      parse.push(CONSTANTS.NEW_LINE)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: \\\\ was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }


  /*
    Not a part of SyntaxAnalyzerTrait, but still needed
   */
  /*
    DOC_BEGIN   ::= '\BEGIN'            //required
   */
  def doc_begin(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOC_BEGIN)){
      parse.push(CONSTANTS.DOC_BEGIN)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: \\BEGIN was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  /*
    DOC_END     ::= '\END'              //required
   */
  def doc_end(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOC_END)){
      parse.push(CONSTANTS.DOC_END)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: \\END was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  /*
    PARA_BEGIN  ::= '\PARB'
   */
  def para_begin(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARA_BEGIN)){
      parse.push(CONSTANTS.PARA_BEGIN)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: \\PARB was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  /*
    PARA_END    ::= '\PARE'
   */
  def para_end(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARA_END)){
      parse.push(CONSTANTS.PARA_END)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: \\PARE was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  /*
    BRACK_BEGIN ::= '['
   */
  def brack_begin(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACK_BEGIN)){
      parse.push(CONSTANTS.BRACK_BEGIN)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: [ was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  /*
    BRACK_END   ::= ']'
   */
  def brack_end(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACK_END)){
      parse.push(CONSTANTS.BRACK_END)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: ] was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  /*
    PAREN_BEGIN ::= '('
   */
  def paren_begin(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PAREN_BEGIN)){
      parse.push(CONSTANTS.PAREN_BEGIN)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: ( was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  /*
    PAREN_END   ::= ')'
   */
  def paren_end(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PAREN_END)){
      parse.push(CONSTANTS.PAREN_END)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: ) was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  /*
    BOLD_BEGIN  ::= '* '
   */
  def bold_begin(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD_BEGIN)){
      parse.push(CONSTANTS.BOLD_BEGIN)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: * was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  /*
    BOLD_END    ::= ' *'
   */
  def bold_end(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD_END)){
      parse.push(CONSTANTS.BOLD_END)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: * was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  /*
    EQUALS      ::= '='
   */
  def equals(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQUALS)){
      parse.push(CONSTANTS.EQUALS)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: = was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  /*
    HTH_TEXT    ::= Plain text
   */
  def hth_text(): Unit = {
    if(Compiler.Scanner.isText(Compiler.currentToken)){
      parse.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: Text was expected when '" + Compiler.currentToken + "' was found.")
      setError()
    }
  }

  /*
    TEXT        ::= Plain text | ε
   */
  def text(): Unit = {
    if(Compiler.Scanner.isText(Compiler.currentToken)){
      parse.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      resetError() //way to deal with epsilon???
    }
  }
}
