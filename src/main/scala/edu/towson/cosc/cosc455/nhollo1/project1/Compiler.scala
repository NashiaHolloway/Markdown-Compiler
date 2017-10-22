package edu.towson.cosc.cosc455.nhollo1.project1

object Compiler {
  var currentToken : String = ""
  var fileContents : String = ""

  val Scanner = new LexicalAnalyzer
  val Parser = new SyntaxAnalyzer
  val SemanticAnalyzer = new SyntaxAnalyzer

  def main(args: Array[String]): Unit = {
    checkFile(args)
    readFile(args(0))

    Scanner.getNextToken()

  }

  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  def checkFile(args : Array[String]) = {
    if (args.length != 1) {
      println("USAGE ERROR: wrong number of args fool!")
      System.exit(1)
    }
    else if (! args(0).endsWith(".mkd")) {
      println("USAGE ERROR: wrong extension fool!")
      System.exit(1)
    }
  }
}
