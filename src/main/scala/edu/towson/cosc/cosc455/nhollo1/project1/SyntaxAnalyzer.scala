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

  override def gittex(): Unit = {
    doc_begin()
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEF)){
      var_def()
    }
    title()
    body()
    doc_end()
  }

  override def para(): Unit = {
    para_begin()
    while(!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARA_END)){
      var_def() //if used, it must be the first
      none_or_more()
    }
    para_end()
  }

  override def link(): Unit = {
    brack_begin()
    hth_text()
    brack_end()
    paren_begin()
    hth_text()
    paren_end()
  }

  override def bold(): Unit = {
    bold_begin()
    hth_text()
    bold_end()
  }

  override def body(): Unit = {
    //If \DEF is used in the body, it must me the first thing
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEF)){
      var_def()
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
    else {
      inner()
      body()
    }
  }

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
      getError()
    }
  }

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
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEW_LINE)){
      new_line()
      inner()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TEXT)){
      text()
      inner()
    }
    else {
      getError()
    }
  }

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
    text()
    equals()
    text()
    brack_end()
  }

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
    Not a part of SyntaxAnalyzerTrait, but still needed
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
