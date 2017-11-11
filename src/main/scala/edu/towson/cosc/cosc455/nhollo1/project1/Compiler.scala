package edu.towson.cosc.cosc455.nhollo1.project1

import java.awt.Desktop
import java.io.{File, IOException}
import scala.io.Source

object Compiler {
  var currentToken : String = ""
  var fileContents : String = ""
  var file: String = ""
  var end: Boolean = false //to help determine end of file

  val Scanner = new LexicalAnalyzer
  val Parser = new SyntaxAnalyzer
  val SemanticAnalyzer = new SemanticAnalyzer

  var fileLength: Int = -1

  def main(args: Array[String]): Unit = {
    //initialize filename
    file = args(0)

    //checks the type of file and puts it into a string
    checkFile(args)
    readFile(args(0))

    fileLength = fileContents.length
    //pass the string to the lexical analyzer!
    Scanner.getNextToken()
    Parser.gittex()

    /*USED TO TEST LEXICAL ANALYZER
    while(fileContents != ""){
      println(currentToken)
      Scanner.getNextToken()
    }
    println(currentToken)
    */


  }

  def readFile(File : String): Unit = {
    val source = scala.io.Source.fromFile(File)
    fileContents = try source.mkString finally source.close()
  }

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

