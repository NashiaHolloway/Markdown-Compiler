package edu.towson.cosc.cosc455.nhollo1.project1

trait LexicalAnalyzerTrait {
  //Unit is the same as void in Scala
  def addChar() : Unit
  def getChar() : Unit
  def getNextToken() : Unit
  def lookup(s : String) : Boolean

}
