package edu.towson.cosc.cosc455.nhollo1.project1

/**
  * Created by Nashia Holloway
  * COSC455-Project 1
  * 11/14/17
  */

import java.awt.Desktop
import java.io.{File, IOException}
import java.io._
import scala.collection.mutable //for stack


class SemanticAnalyzer {

  var count: Int = 0
  var range: Int = 0
  var varName = new mutable.Queue[String]()
  var varDefninit = new mutable.Queue[String]()  //< ^ easier to add and delete from the "Symbol Table"
  var stack = new mutable.Stack[String]()

  var linkText: String = ""
  var link: String = ""

  /**
    * Translates gittex language, that has passes the lexical and syntax analyzers, into HTML
    */
  def outputHTML(): Unit = {
    val file = new PrintWriter(new File("outputHTML.html"))
    //push parse tree from syntax analyzer into stack for translation
    for(c <- Compiler.Parser.parse){
      stack.push(c)
    }
    while(stack.nonEmpty){
      var currentToken: String = stack.pop()
      currentToken.toUpperCase() match {

        /**
          * DOC_BEGIN   ::= <html>
          *   DOC_END   ::= </html>
          */
        case CONSTANTS.DOC_BEGIN => file.append("<html>\n")
        case CONSTANTS.DOC_END => file.append("\n</html>")

        /**
          * TITLE   ::= <head><title>has to have text</title></head>
          */
        case CONSTANTS.TITLE => file.append("<head>\n<title> ")
          currentToken = stack.pop()
          //have to get text and end bracket for \TITLE[
          while(!currentToken.equalsIgnoreCase(CONSTANTS.BRACK_END)){
            file.append(currentToken + " ") //space for text
            currentToken = stack.pop()
          }
          file.append(" </title>\n</head>\n")

        /**
          * HEAD    ::= <h1>has to have text</h1>
          */
        case CONSTANTS.HEAD => file.append("<h1> ")
          currentToken = stack.pop()
          //have to get text for \HEAD, until current token is an "important token"
          while(!Compiler.Scanner.importantTokens.contains(currentToken)){
            file.append(currentToken + " ")
            currentToken = stack.pop()
          }
          stack.push(currentToken)
          file.append(" </h1>\n")

        /**
          * PARA_BEGIN    ::= <p>
          *   PARA_END    ::= </p>
          */
        case CONSTANTS.PARA_BEGIN => file.append("<p> ")
          range = 1
        case CONSTANTS.PARA_END => file.append(" </p>\n")
          if(range == 1 && count > 0){
            for(c <- 0 until count){
              varName.dequeue()
            }
          }

        /**
          * LINK_BEGIN    ::= <a href="some text">has to have text</a>
          */
        case CONSTANTS.LINK_BEGIN =>
          linkText = ""
          link = ""
          currentToken = stack.pop()
          while(!currentToken.equals(CONSTANTS.BRACK_END)){
            linkText = linkText + currentToken + " "
            currentToken = stack.pop()
          }
          stack.pop()
          link += stack.pop()
          stack.pop()
          file.append("<a href=\"" + link + "\">" + linkText + "</a> ")

        /**
          * UL    ::= <li> can be USE, BOLD, LINK, or text</li>
          */
        case CONSTANTS.UL => file.append("\n<li> ")
          currentToken = stack.pop()
          if(currentToken.contains("\n")){
            file.append(currentToken + " </li>")
          }
          else if(currentToken.equalsIgnoreCase(CONSTANTS.USE)){
            stack.push(currentToken)
          }
          else {
            if(!currentToken.equalsIgnoreCase(CONSTANTS.USE)){
              file.append(currentToken + " ")
              currentToken = stack.top
              if(currentToken.contains("\n")){
                currentToken = stack.pop()
                file.append(currentToken + " </li>")
              }
            }
          }

        /**
          * NEW_LINE    ::= <br>
          */
        case CONSTANTS.NEW_LINE => file.append("<br>\n")

        /**
          * IMG   ::= <img src="some text" alt="some more text">
          */
        case CONSTANTS.IMG =>
          linkText = ""
          link = ""
          currentToken = stack.pop()
          while(!currentToken.equals(CONSTANTS.BRACK_END)){
            linkText = linkText + currentToken + " "
            currentToken = stack.pop()
          }
          stack.pop()
          link += stack.pop()
          stack.pop()
          file.append("<img src=\"" + link + "\" alt=" + linkText + "\">")

        /**
          * BOLD    ::= <b>has to have text</b>
          */
        case CONSTANTS.BOLD =>
          var boldText: String = ""
          currentToken = stack.pop()
          while(!currentToken.equals(CONSTANTS.BOLD)){
            boldText += currentToken
            currentToken = stack.pop()
          }
          file.append("<b> " + boldText + " </b>")

        /**
          * DEF   ::= add and delete variables from queue here
          */
        case CONSTANTS.DEF =>
          varName.enqueue(stack.pop())
          stack.pop()
          varDefninit.enqueue(stack.pop())
          stack.pop()
          if(range == 1){
            count += 1
          }

        /**
          * USE   ::= only use variables that were previously defined here
          * Throw a semantic error, if variables were used and not defined.
          */
        case CONSTANTS.USE =>
          var useDefVar: String = stack.pop()
          if(varName.contains(useDefVar)){
            file.append(varDefninit(varName.indexOf(useDefVar, range)) + " ")
          }
          else {
            println("STATIC SEMANTIC ERROR: " + useDefVar + " was never defined.")
            System.exit(1)
          }
          stack.pop()

        /**
          * For text, basically
          */
        case _ => file.append(currentToken + " ")
      }
    }
    file.close() //end file
    openHTMLFileInBrowser("outputHTML.html") //open webpage!!!
  }


  /**
    * Hack Scala/Java function to take a
    * String filename and open in default web browser.
    * @param htmlFileStr
    */
  def openHTMLFileInBrowser(htmlFileStr : String): Unit = {
    val file : File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
      case e: Exception => sys.error("He's dead, Jim!")
    }
  }
}


