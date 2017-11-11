package edu.towson.cosc.cosc455.nhollo1.project1

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

  override def none_or_more(): Unit = ???

  override def inner(): Unit = {

  }

  override def link(): Unit = ???

  /**
    * OPTIONAL
    * <body>  ::= <inner> <body>
    *         | <para> <body>
    *         | <new_line> <body>
    *         | ε
    */
  override def body(): Unit = {
    if(Compiler.fileLength == Compiler.Scanner.filePos){
      //do nothing
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

  override def bold(): Unit = ???

  override def new_line(): Unit = ???

  /**
    * NOT OPTIONAL
    * <title>     ::= TITLE HTH_TEXT BRACK_END
    */
  override def title(): Unit = {
    titleHelper()
    hth_text()
    brackeHelper()
  }

  override def var_def(): Unit = ???

  override def var_use(): Unit = ???

  override def img(): Unit = ???

  override def head(): Unit = ???

  override def ul(): Unit = ???

  /*
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
    * HTH_TEXT    ::= Plain text
    */
  def hth_text(): Unit = {
    if(isTextParser && !Compiler.currentToken.equals(CONSTANTS.SPEC)){
      parse.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      hth_text()
    }
    else {
      println("SYNTAX ERROR: Text was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }

  def isTextParser: Boolean = {
    checkIt = false
    if(!Compiler.currentToken.equals(CONSTANTS.SPEC)){
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
  /*
  //For errors and helper methods
  var errorFound : Boolean = false
  def setError() = errorFound = true
  def resetError() = errorFound = false
  def getError() : Boolean = errorFound

  /**
    * <gittex>  ::= DOC_BEGIN <var_def> <title> <body> DOC_END
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

  /**
    * OPTIONAL
    * <body>  ::= <inner> <body>
    *         | <para> <body>
    *         | <new_line> <body>
    *         | ε
    */
  override def body(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEF)) {
      var_def()
      body()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEAD)){
      head()
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
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOC_END)){

    }
    else {
      inner()
      //body()
    }
  }

  /**
    * NOT OPTIONAL
    * <title>     ::= TITLE HTH_TEXT BRACK_END
    */
  override def title(): Unit = {
    Title()
    hth_text()
    brack_end()
  }

  /**
    * OPTIONAL
    * <head>      ::= HEAD HTH_TEXT | ε
    */
  override def head(): Unit = {
    Head()
    hth_text()
  }

  /**
    * <para>      ::= PARB <var_def> <inner> PARE
    */
  override def para(): Unit = {
    para_begin()
    var_def()
    while(!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOC_END) && !Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARA_END)){
      inner()
    }
    para_end()
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
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USE)){
      var_use()
      none_or_more()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      bold()
      none_or_more()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINK_BEGIN)){
      link()
      none_or_more()
    }
      /*
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HTH_TEXT)){
      text()
      none_or_more()
    }
    */
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOC_END)){
      //setError() //returns true (replace with "found = true"?)
    }
    else if(Compiler.Scanner.isText(Compiler.currentToken.toCharArray)){
      hth_text()
      none_or_more()
    }
    else {
      //getError() //no error, since it's optional, just leave the method
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
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARA_END)){
      if (parse.contains(CONSTANTS.PARA_BEGIN)){
        para_end()
      }
      else {
        println("Syntax Error: \\PARAB was never given")
        System.exit(1)
      }
    }
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEAD)){
      head()
      inner()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)){
      bold()
      inner()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.UL)){
      ul()
      inner()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINK_BEGIN)){
      link()
      inner()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMG)){
      img()
      inner()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USE)){
      var_use()
      inner()
    }
    else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEW_LINE)) {
      new_line()
      inner()
    }
    else if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOC_END)){

    }
    else {
      text()
    }

  }

  /**
    * OPTIONAL
    * <var_def>   ::= DEF HTH_TEXT EQUALS HTH_TEXT BRACK_END <var_def> | ε
    */
  override def var_def(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEF)){
      varDef()
      hth_text()
      equals()
      hth_text()
      brack_end()
    }
  }

  /**
    * OPTIONAL
    * <var_use>   ::= USE HTH_TEXT BRACK_END | ε
    */
  override def var_use(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USE)){
      varUse()
      hth_text()
      brack_end()
    }
  }

  /**
    * OPTIONAL
    * <bold>      ::= BOLD TEXT BOLD | ε
    */
  override def bold(): Unit = {
    Bold()
    text()
    Bold()
  }

  /**
    * OPTIONAL
    * <ul>        ::= UL <none_or_more> <ul> | ε
    */
  override def ul(): Unit = {
    list()
    none_or_more()
  }

  /**
    * OPTIONAL
    * <link>      ::= LINK_BEGIN HTH_TEXT BRACK_END PAREN_BEGIN HTH_TEXT PAREN_END | ε
    */
  override def link(): Unit = {
    link_begin()
    hth_text()
    brack_end()
    paren_begin()
    hth_text()
    paren_end()
  }

  /**
    * <img>       ::= IMG HTH_TEXT BRACK_END PAREN_BEGIN HTH_TEXT PAREN_END | ε
    */
  override def img(): Unit = {
    image()
    hth_text()
    brack_end()
    paren_begin()
    hth_text()
    paren_end()
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






  /*
    Not a part of SyntaxAnalyzerTrait, but still needed
   */
  /**
    * DOC_BEGIN   ::= '\BEGIN'            //required
    */
  def doc_begin(): Unit = {
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
    * DOC_END     ::= '\END'              //required
    */

  def doc_end(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOC_END)){
      parse.push(CONSTANTS.DOC_END)
      Compiler.Scanner.getNextToken()
      if(Compiler.Scanner.nextChar.equals('\n'))
        return
      else {
        println("SYNTAX ERROR: There shouldn't be text after \\END")
        System.exit(0)
      }
    }
    else {
      println("SYNTAX ERROR: \\END was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }


  /**
    * TITLE       ::= '\TITLE['            //required
    */
  def Title(): Unit = {
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
    * PARA_BEGIN  ::= '\PARB'
    */
  def para_begin(): Unit = {
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
    * PARA_END    ::= '\PARE'
    */
  def para_end(): Unit = {
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
    * LINK_BEGIN ::= '['
    */
  def link_begin(): Unit = {
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
    * BRACK_END   ::= ']'
    */
  def brack_end(): Unit = {
    if(Compiler.currentToken.equals(CONSTANTS.BRACK_END)){
      parse.push(CONSTANTS.BRACK_END)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: ] was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }

  /**
    * PAREN_BEGIN ::= '('
    */
  def paren_begin(): Unit = {
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
  def paren_end(): Unit = {
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
    * BOLD_BEGIN  ::= '* '
    */
  def Bold(): Unit = {
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
    * EQUALS      ::= '='
    */
  def equals(): Unit = {
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
    * HEAD        ::= '# '                //for header
    */
  def Head(): Unit = {
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
  def varDef(): Unit = {
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
    * USE         ::= '\USE['              //for using defined variables
    */
  def varUse(): Unit = {
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
    * UL          ::= '+ '                //for unordered list
    */
  def list(): Unit = {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.UL)){
      parse.push(CONSTANTS.UL)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: + was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }

  /**
    * IMG         ::= '!['                 //for image
    */
  def image(): Unit = {
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
    if(Compiler.Scanner.isText(Compiler.currentToken)){
      parse.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else {
      println("SYNTAX ERROR: Text was expected when '" + Compiler.currentToken + "' was found.")
      System.exit(1)
    }
  }

  /**
    * TEXT        ::= Plain text | ε
    */
  def text(): Unit = {
    if(Compiler.Scanner.isText(Compiler.currentToken)){
      parse.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
    else{

    }
  }
*/

}
