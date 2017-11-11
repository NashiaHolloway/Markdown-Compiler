package edu.towson.cosc.cosc455.nhollo1.project1

/**
  * Created by Nashia Holloway
  * COSC455 Project 1
  * 11/14/17
  */

object Compiler {
  var currentToken : String = ""
  var fileContents : String = ""
  var file: String = ""

  val Scanner = new LexicalAnalyzer
  val Parser = new SyntaxAnalyzer
  val SemanticAnalyzer = new SemanticAnalyzer

  var fileLength: Int = 0

  /**
    * Main method for translation of gittex to html:
    * Compiler reads in file, checks for correct extension.  If wrong extension, throws an error.
    * Lexical Analyzer takes input file as a string and checks for lexical errors.  Once that passes,
    * the Syntax Analyzer takes the lexical tokens and checks for syntax errors and creates a parse tree.
    * Once that passes, the Semantic Analyzer pushes the parse tree into a stack to then translate into html and open
    * an html file in a web browser.  If there is any error, the compiler will throw an error and recover.
    * @param args
    */
  def main(args: Array[String]): Unit = {
    //initialize filename
    file = args(0)

    //checks the type of file and puts it into a string
    checkFile(args)
    readFile(args(0))

    fileLength = fileContents.length

    /*//USED TO TEST LEXICAL ANALYZER
    while(Compiler.Scanner.filePos < fileLength){
      println(currentToken)
      Scanner.getNextToken()
    }
    println(currentToken)
    */

    //loops till empty
    while (Scanner.filePos < fileLength) {
      //pass the file string to the lexical analyzer!
      Scanner.getNextToken()

      //get tokens from lexical analyzer and check for syntax errors
      Parser.gittex()
      if (currentToken.equalsIgnoreCase(CONSTANTS.DOC_END)) {
        //endCase = true
      }
    }
    //if lexical and syntactical analysis is passed, go to semantic analyzer for translation
    SemanticAnalyzer.outputHTML()
  }

  /**
    * Read in file
    * @param File
    */
  def readFile(File : String): Unit = {
    val source = scala.io.Source.fromFile(File)
    fileContents = try source.mkString finally source.close()
  }

  /**
    * Makes sure file has right extension and has at least 1 argument on command line
    * @param args
    */
  def checkFile(args : Array[String]): Unit = {
    if (args.length != 1) {
      println("USAGE ERROR: wrong number of args fool!")
      System.exit(1)
    }
    else if (! args(0).endsWith(".gtx")) {
      println("USAGE ERROR: wrong extension fool!")
      System.exit(1)
    }
  }
}

