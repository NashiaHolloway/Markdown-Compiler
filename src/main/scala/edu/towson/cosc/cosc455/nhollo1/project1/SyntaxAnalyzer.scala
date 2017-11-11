package edu.towson.cosc.cosc455.nhollo1.project1

/**
  * Created by Nashia Holloway
  * COSC455-Project 1
  * 11/14/17
  */

import scala.collection.mutable

class SyntaxAnalyzer extends SyntaxAnalyzerTrait {


  var parse: mutable.Stack[String] = mutable.Stack[String]() //to add to stack "parse.push()"
  var checkIt: Boolean = false

  /**
    * <gittex>  ::= DOC_BEGIN <var_def> <title> <body> DOC_END
    */
  override def gittex(): Unit = {
    docbHelper()
    var_def()
    title()
    body()
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOC_END)){
      parse.push(Compiler.currentToken)
    }
    else{
      println("SYNTAX ERROR: \\END expected, when " + Compiler.currentToken + " was found.")
      System.exit(1)
    }
  }

  /**
    * <para>      ::= PARB <var_def> <inner> PARE
    */
  override def para(): Unit = {
    parabHelper()
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEF)){
      var_def()
    }
    inner()
    paraeHelper()
  }

  /**
    * OPTIONAL
    * //same as <innter-item>
    * <none_or_more>  ::= <var_use> <none_or_more>
    *                 | <bold> <none_or_more>
    *                 | <link> <none_or_more>
    *                 | HTH_TEXT <none_or_more>
    *                 | ε
    */
  override def none_or_more(): Unit = {
    //can't use match, because var_use might be upper or lowercase... Can't figure that out with match
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USE)){
      var_use()
      none_or_more()
    }
    else if(Compiler.currentToken.equals(CONSTANTS.BOLD)){
      bold()
      none_or_more()
    }
    else if(Compiler.currentToken.equals(CONSTANTS.LINK_BEGIN)){
      link()
      none_or_more()
    }
    else if(isTextParser()){
      hth_text()
      none_or_more()
    }
    else {
      //nun
    }
  }

  /**
    * OPTIONAL
    * //same as <inner-text>
    * <inner>   ::= <var_use> <inner>
    *           | <head> <inner>
    *           | <bold> <inner>
    *           | <ul> <inner>
    *           | <link> <inner>
    *           | <img> <inner>
    *           | TEXT <inner>
    *           | ε
    */
  override def inner(): Unit = {
    //don't need .equalsIgnoreCase, b/c none of the parameters have text, and it doesn't matter if "text" has lowercase letters
    Compiler.currentToken match {
      case CONSTANTS.USE => var_use(); inner()
      case CONSTANTS.HEAD => head(); inner()
      case CONSTANTS.BOLD => bold(); inner()
      case CONSTANTS.UL => ul(); inner()
      case CONSTANTS.LINK_BEGIN => link(); inner()
      case CONSTANTS.IMG => img(); inner()
      case _ => if(isTextParser()){
        parse.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        inner()
      }
    }
  }

  /**
    * OPTIONAL
    * <link>      ::= LINK_BEGIN HTH_TEXT BRACK_END PAREN_BEGIN HTH_TEXT PAREN_END | ε
    */
  override def link(): Unit = {
    linkHelper()
    hth_text()
    brackeHelper()
    parenbHelper()
    hth_text()
    pareneHelper()
  }

  /**
    * OPTIONAL
    * <body>  ::= <inner> <body>
    *         | <para> <body>
    *         | <new_line> <body>
    *         | ε
    */
  override def body(): Unit = {
    if(Compiler.fileLength == Compiler.Scanner.filePos){
      //nun
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARA_BEGIN) || Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARA_END)){
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

  /**
    * OPTIONAL
    * <bold>      ::= BOLD TEXT BOLD | ε
    */
  override def bold(): Unit = {
    boldHelper()
    hth_text()
    boldHelper()
  }

  /**
    * <new_line>  ::= NEW_LINE | ε
    */
  override def new_line(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEW_LINE)){
      parse.push(CONSTANTS.NEW_LINE)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: \\\\ was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }

  /**
    * NOT OPTIONAL
    * <title>     ::= TITLE HTH_TEXT BRACK_END
    */
  override def title(): Unit = {
    titleHelper()
    hth_text()
    brackeHelper()
  }

  /**
    * OPTIONAL
    * <var_def>   ::= DEF HTH_TEXT EQUALS HTH_TEXT BRACK_END <var_def> | ε
    */
  override def var_def(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEF)){
      vardefHelper()
      hth_text()
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQUALS)){
        equalsHelper()
        hth_text()
        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACK_END)){
          brackeHelper()
          var_def() //recurse
        }
      }
    }
  }

  /**
    * OPTIONAL
    * <var_use>   ::= USE HTH_TEXT BRACK_END | ε
    */
  override def var_use(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USE)){
      varuseHelper()
      hth_text()
      brackeHelper()
    }
  }

  /**
    * <img>       ::= IMG HTH_TEXT BRACK_END PAREN_BEGIN HTH_TEXT PAREN_END | ε
    */
  override def img(): Unit = {
    imgHelper()
    hth_text()
    brackeHelper()
    parenbHelper()
    hth_text()
    pareneHelper()
  }

  /**
    * OPTIONAL
    * <head>      ::= HEAD HTH_TEXT | ε
    */
  override def head(): Unit = {
    headHelper()
    hth_text()
  }

  /**
    * OPTIONAL
    * <ul>        ::= UL <none_or_more> <ul> | ε
    */
  override def ul(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.UL)) {
      parse.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      none_or_more()
      ul() //recurse
    }
  }






  /*
    HELPER METHODS
    Not a part of SyntaxAnalyzerTrait, but still needed
   */
  /**
    * DOC_BEGIN   ::= '\BEGIN'            //required
    */
  def docbHelper(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOC_BEGIN)){
      parse.push(CONSTANTS.DOC_BEGIN)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: \\BEGIN was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }

  /**
    * TITLE       ::= '\TITLE['            //required
    */
  def titleHelper(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLE)) {
      parse.push(CONSTANTS.TITLE)
      Compiler.Scanner.getNextToken()
    }
    else {
      print("SYNTAX ERROR: \\TITLE[ was expected when '" + Compiler.currentToken + "' was found." )
      System.exit(1)
    }
  }

  /**
    * BRACK_END   ::= ']'
    */
  def brackeHelper(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACK_END)){
      parse.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: ] needed for every [ given. " + Compiler.currentToken + " was found.")
      System.exit(1)
    }
  }

  /**
    * PARA_BEGIN  ::= '\PARAB'
    */
  def parabHelper(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARA_BEGIN)){
      parse.push(CONSTANTS.PARA_BEGIN)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: \\PARAB was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }

  /**
    * PARA_END    ::= '\PARAE'
    */
  def paraeHelper(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARA_END)){
      parse.push(CONSTANTS.PARA_END)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: \\PARAE was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }

  /**
    * HEAD        ::= '# '                //for header
    */
  def headHelper(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEAD)){
      parse.push(CONSTANTS.HEAD)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: # was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }

  /**
    * DEF         ::= '\DEF['              //for defining variables
    */
  def vardefHelper(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEF)){
      parse.push(CONSTANTS.DEF)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: \\DEF[ was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }

  /**
    * EQUALS      ::= '='
    */
  def equalsHelper(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQUALS)){
      parse.push(CONSTANTS.EQUALS)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: = was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }

  /**
    * USE         ::= '\USE['              //for using defined variables
    */
  def varuseHelper(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USE)){
      parse.push(CONSTANTS.USE)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: \\USE[ was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }

  /**
    * BOLD  ::= '*'
    */
  def boldHelper(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      parse.push(CONSTANTS.BOLD)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: * was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }

  /**
    * LINK_BEGIN ::= '['
    */
  def linkHelper(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINK_BEGIN)){
      parse.push(CONSTANTS.LINK_BEGIN)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: [ was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }

  /**
    * PAREN_BEGIN ::= '('
    */
  def parenbHelper(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PAREN_BEGIN)){
      parse.push(CONSTANTS.PAREN_BEGIN)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: ( was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }

  /**
    * PAREN_END   ::= ')'
    */
  def pareneHelper(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PAREN_END)){
      parse.push(CONSTANTS.PAREN_END)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: ) was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }

  /**
    * IMG         ::= '!['                 //for image
    */
  def imgHelper(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMG)){
      parse.push(CONSTANTS.IMG)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: ![ was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }






  /**
    * HTH_TEXT    ::= Plain text
    */
  def hth_text(): Unit = {
    if(isTextParser() && !Compiler.currentToken.contains(CONSTANTS.SPEC)){
      parse.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      hth_text()
    }
  }

  def isTextParser(): Boolean = {
    checkIt = false
    if(Compiler.currentToken.filter(c => CONSTANTS.SPEC.contains(c)).length == 0){
      checkIt = Compiler.currentToken.contains(',') || Compiler.currentToken.contains(':') || Compiler.currentToken.contains('.')
      if(!checkIt){
        if(Compiler.currentToken.last == '\n'){
          checkIt = Compiler.currentToken.substring(0, Compiler.currentToken.length - 1).last.isLetterOrDigit
        }
        else {
          checkIt = Compiler.currentToken.last.isLetterOrDigit
        }
      }
    }
    checkIt
  }
}
