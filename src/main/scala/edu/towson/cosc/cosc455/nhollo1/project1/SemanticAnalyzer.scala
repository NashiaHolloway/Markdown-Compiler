package edu.towson.cosc.cosc455.nhollo1.project1

import java.awt.Desktop
import java.io.{File, IOException}
import java.io._
import scala.collection.mutable //for parse tree stack


class SemanticAnalyzer {
  //declare variables/values
  var parseTree: mutable.Stack[String] = mutable.Stack[String]()
  var outputTree: mutable.Stack[String] = mutable.Stack[String]()
  var temp: String = ""
  var next: String = ""
  var varName = new Array[String](10)
  var varDefinit = new Array[String](10)
  var count: Int = 0
  var printed: Boolean = false


  /*
    Starter method
   */
  def semantics(): Unit = {
    //prep
    parseTree = Compiler.Parser.parse.reverse
    next = parseTree.pop()

    //method to process the file
    outputHTML()
  }

  /*
    translates input file to HTML text
   */
  def outputHTML(): Unit = {
    while(!parseTree.isEmpty){
      next match {
        case CONSTANTS.DOC_BEGIN =>           //\BEGIN
          outputTree.push("<html>")
          next = parseTree.pop()

        case CONSTANTS.TITLE =>               //\TITLE[
          outputTree.push("<head>")
          outputTree.push("<title>")
          outputTree.push(parseTree.pop())
          outputTree.push("</title>")
          outputTree.push("</head>")
          parseTree.pop()
          next = parseTree.pop()

        case CONSTANTS.HEAD =>                //#
          outputTree.push("<h1>")
          outputTree.push(parseTree.pop())
          outputTree.push("</h1>")
          next = parseTree.pop()

        case CONSTANTS.PARA_BEGIN =>          //\PARB
          outputTree.push("<p>")
          parseTree.pop()

        case CONSTANTS.PAREN_END =>           //\PARE
          outputTree.push("</p>")
          next = parseTree.pop()

        case CONSTANTS.BOLD => //*
          outputTree.push("<b>")
          outputTree.push(parseTree.pop())
          outputTree.push("</b>")
          parseTree.pop()
          next = parseTree.pop()

        case CONSTANTS.UL =>                  //+
          outputTree.push("<li>")
          next = parseTree.pop()
          if (next.contains("\n") && !parseTree.isEmpty && !next.equalsIgnoreCase(CONSTANTS.DOC_END))
            outputTree.push(next)
          else if (!next.equalsIgnoreCase(CONSTANTS.DOC_END))
            outputHTML()
          outputTree.push("</li>")
          if(!parseTree.isEmpty)
            next = parseTree.pop()

        case CONSTANTS.NEW_LINE =>            //\\
          outputTree.push("<br>")
          next = parseTree.pop()

        case CONSTANTS.LINK_BEGIN =>          //[
          val temp = parseTree.pop()
          parseTree.pop()
          parseTree.pop()
          next = parseTree.pop()
          parseTree.pop()

          outputTree.push("<a href = \"")
          outputTree.push(next)
          outputTree.push("\">")
          outputTree.push(temp)
          outputTree.push("<a/>")
          next = parseTree.pop()

        case CONSTANTS.IMG =>                 //![
          val temp = parseTree.pop()
          parseTree.pop()
          parseTree.pop()
          next = parseTree.pop()
          parseTree.pop()

          outputTree.push("<img src =\"")
          outputTree.push(next)
          outputTree.push(temp)
          outputTree.push("\">")
          next = parseTree.pop()

        case CONSTANTS.DEF =>                //\DEF[
          var name = parseTree.pop()
          parseTree.pop()
          val definit = parseTree.pop()
          parseTree.pop()
          name = name.filter(!" ".contains(_))
          val defined = varName.indexOf(name)
          if(defined != -1) {
            varName(defined) = name
            varDefinit(defined) = definit
          }
          else {
            varName(count) = name
            varDefinit(count) = definit
            count += 1
          }
          next = parseTree.pop() //if nothing to push, pop

        case CONSTANTS.USE =>               //\USE[
          var name = parseTree.pop()
          parseTree.pop()
          name = name.filter(!" ".contains(_))
          if(varName.contains(name))
            outputTree.push(varDefinit(varName.indexOf(name)))
          else{
            println("Static Symantic Error: Variable has not been defined!")
            System.exit(1)
          }
          next = parseTree.pop()

        case CONSTANTS.DOC_END =>           //\END
          outputTree.push("</html>")

        case _ =>
          outputTree.push(next)
          next = parseTree.pop()
      }

      /*
        Print output to file
       */
      val out = outputTree.reverse.mkString
      val print = new PrintWriter(new File(Compiler.fileContents + ".html"))
      print.write(out)
      print.close

      //open HTML
      if(!printed){
        openHTMLFileInBrowser(Compiler.fileContents + ".html")
        printed = true
      }
    }
  }

  /*
    Hack Scala/Java function to take a
    String filename and open in default web browser.
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
