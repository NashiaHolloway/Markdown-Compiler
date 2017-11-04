# Markdown-Compiler
This compiler translates a self-created programming language (Gittex Markdown language) into HTML.

# Goals
The intention of this project is to use and apply the concepts of programming languages to design and develop 
a complex compiler.  In doing so, I have developed a better understanding of the design of a high-level programming
language and the need for studying the concepts behind programming languages.  Additionally, through this project, I
have familiarized myself with several software development tools (e.g., IntelliJ, GitHub, etc.), Scala programming
language tools (e.g., Scaladocs, Scala tests, ScalaCheck, etc.) and gained experience in software engineering and 
development.

This project has given me developmental experience in the following areas of programming languages/concepts:
  Writing BNF/EBNF grammar rules
  Lexical analysis
  Syntactical analysis and parsing
  Semantics
  Static-scope analysis
  
  OOP design and development
  Method invocation
  Exception handling
  Recursion
  Configuration management
  Unit testing
  
 # Description
 I have implemented a character by character lexical analyzer that partitions the lexemes of a source file in the
 Gittex Markdown language into tokens.  To do this, I extended the Lexical Analyzer Scala trait (similar to Java Interface).
 Any lexical errors encountered are repeorted as output to the console with as much error information as possible.
 The compiler will exit after the first error is encountered.  If an error is encountered, no output file will be created.
 
 Next, I implemented a recursive-descent parser (syntax analyzer) that builds an abstract syntax tree (parse tree). 
 To do this, I extended the Syntax Analyzer Scala trait.  Any syntax errors are reported as output to the console with as
 much information as possible.  The compiler will exit after the first error is encountered.  If an error is encountered, no 
 output file will be created.
 
 I then implemented the variable solution for the Gittex Markdown language.  Any semantic errors encountered are reported
 as output to the console with as much error information as possible.  The comiler will exit after the first error is
 encountered.  If an error is encountered, no output file will be created.
 
 Finally, I implemented the semantic analyzer that takes the abstract syntax tree and translates it to a lower level
 language — HTML5.
 
 # Execution
 The final program takes one command-line argument provided by the user: an inpus file name in the Gittex Markdown language.
 All Gittex Markdown source files are required to have a gtx extension.  The compiler then generates an "executable" output 
 file with the same name but an html extension and be viewable in the Google Chrome 60+ browser.
 Example:
  C:\> java -jar compiler.jar Test1.gtx
  
 


