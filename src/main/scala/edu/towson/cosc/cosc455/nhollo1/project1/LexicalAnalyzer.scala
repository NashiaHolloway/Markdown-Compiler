package edu.towson.cosc.cosc455.nhollo1.project1

class LexicalAnalyzer extends LexicalAnalyzerTrait {
  //declare values
  override def addChar(): Unit = ???

  override def lookup(): Boolean = ???

  override def getNextToken(): Unit = {
    val c  = getChar()
  }

  override def getChar(): Char = ???

  //checks if there is text, or not
  def isText(text: String): Boolean = {
    if (text.contains("\\"))
      return false
    else
      return true
  }

}
