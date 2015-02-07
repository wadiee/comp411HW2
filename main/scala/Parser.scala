/** Parser for Assignment 1 */

import java.io._

import scala.collection.mutable.ArrayBuffer

class Parser(in: Lexer) {
  
  /** Convenience constructor that takes a Reader as input */
  
  def this(inputStream: Reader) { this(new Lexer(inputStream)) }
  
  def this(fileName: String) { this(new FileReader(fileName)) }
  
  def lexer: Lexer = in
  
  def parse():AST = {
    val prog = parseExp()
//    System.err.println("top level parseExp() returned '" + prog + "'")
    val t = in.readToken()
    if (t eq null) prog
    else throw new ParseException("Legal program followed by extra token " + t)
  }
  
  /** Parses:
    *   <exp> :: = if <exp> then <exp> else <exp>
    *            | let <prop-def-list> in <exp>
    *            | map <id-list> to <exp>
    *            | <term> { <biop> <exp> } 
    */
  private def parseExp():AST = {
    val token = in.readToken()
    
//    System.err.println("parseExp called first token is " + token)
    
    if (token eq in.IF) parseIf()  // in is the Lexer
//    else if (token eq in.LETREC) parseLetRec()  // supports addition of letrec
    else if (token eq in.LET) parseLet()
    else if (token eq in.MAP) parseMap()
    
    /*  Supports the addition of blocks to Jam */
//    else if (token eq LeftBrace) {
//      val exps = parseExps(SemiColon,RightBrace)  
//      // including closing brace
//      if (exps.length eq 0) throw new ParseException("Illegal empty block")
//      else Block(exps)
//    }
    
    else {
      
      /* phrase begins with a term */
      var exp:AST = parseTerm(token)
      
      var next: Token = in.peek()
      while (next.isInstanceOf[OpToken]) {
        val op: OpToken = next.asInstanceOf[OpToken]
        in.readToken() // remove next from input stream
        
        if (! op.isBinOp) error(next, "binary operator")
        
        val newTerm = parseTerm(in.readToken())
        exp = BinOpApp(op.binOp, exp, newTerm)
//          System.err.println("exp updated to: " + exp)
        next = in.peek()
      }
//      System.err.println("parseTerm returning " + exp)
      exp
    }
  }
  
  /** Parses:
    *  <term>     ::= { <unop> } <term> | <constant> | <factor> {( <exp-list> )}
    *  <constant> ::= <empty> | <int> | <bool>
    * @param token   first token in input stream to be parsed; remainder in Lexer named in.
    */
  private def parseTerm(token: Token): AST = {
    
    if (token.isInstanceOf[OpToken]) {
      val op = token.asInstanceOf[OpToken]
      if (! op.isUnOp) error(op,"unary operator")
      else UnOpApp(op.unOp, parseTerm(in.readToken()))
    }
    
    else if (token.isInstanceOf[Constant]) token.asInstanceOf[Constant]
    else {
      val factor = parseFactor(token)
      val next = in.peek()
      if (next eq LeftParen) {
        in.readToken()  // remove next from input stream
        val exps = parseArgs()  // including closing paren
        App(factor, exps)
      }
      else factor
    }
  }
  
  /** Parses:
    *  <factor>   ::= <prim> | <variable> | ( <exp> )
    * @param token   first token in input stream to be parsed; remainder in Lexer named in.
    */
  private def parseFactor(token: Token): AST = {
    
//    System.err.println("parseFactor(" + token + ") called")
    
    if (token eq LeftParen) {
      val exp = parseExp()
      val next = in.readToken()
      if (next != RightParen) error(next,"`)'")
      else exp
    }
    
    else if (! (token.isInstanceOf[PrimFun]) && ! (token.isInstanceOf[Variable]))
      error(token,"constant, primitive, variable, or `('")
    
    // Term\Constant = Variable or PrimFun       
    else token.asInstanceOf[Term]
  }      
  
  /** Parses `if <exp> then <exp> else <exp>' given that `if' has already been read. */
  private def parseIf(): If = {
    
    val test = parseExp()
    val key1 = in.readToken()
    if (key1 != in.THEN) error(key1,"`then'")
    val conseq = parseExp()
    val key2 = in.readToken()
    if (key2 != in.ELSE) error(key2,"`else'")
    val alt = parseExp()
    If(test, conseq, alt)
  }
  
  /** Parses `let <prop-def-list> in <exp>' given that `let' has already been read. */ 
  private def parseLet(): Let = {
    val defs = parseDefs(false) 
    // consumes `in'; false means rhs may be an arbitrary Jam expression, not just a MapLiteral (required in letrec)
    val body = parseExp()
    Let(defs,body)
  }
  
  /* Supports the parsing of 'letrec' */
//  /** Parses `letrec <prop-def-list> in <exp>' given that `letrec' has already been read. */
//  private AST parseLetRec() {
//    
//    Def[] defs = parseDefs(true);
//    // consumes `in'; true means each rhs must be a MapLiteral
//    AST body = parseExp();
//    return new LetRec(defs,body);
//  }
  
  /* Parses `map <id-list> to <exp>' given that `map' has already been read. */
  private def parseMap():MapLiteral = {
    
//   System.err.println("parseMap called; next token is '" + in.peek() + "'")
    
    val vars: Array[Variable] = parseVars()  // consumes the delimiter `to'
//    System.err.println("In parseMap after parsing variables, vars = '" + Arrays.toString(vars.asInstanceOf[Array[java.lang.Object]]) + 
//                       "'; next token is '" + in.peek() + "'")
    val body = parseExp()
//    System.err.println("In parseMap, body = '" + body + "'")
    val result = MapLiteral(vars, body)
//    System.err.println("parseMap returning '" + result + "'")
    result
  }
  
  /** Parses '<exp-list> <delim>' where
    *  <exp-list>      ::= <empty> | <prop-exp-list>
    *  <empty> ::=
    *  <prop-exp-list> ::= <exp> | <exp> <separator> <prop-exp-list> 
    */
  private def parseExps(separator: Token, delim: Token): Array[AST] = {
    
    val exps: ArrayBuffer[AST] = new ArrayBuffer()
    var next = in.peek()
    
    if (next eq delim) {
      in.readToken() // consume RightParen
      new Array[AST](0)
    }
    else {
      // next is still at front of input stream
      
      do {
        val exp = parseExp()
        exps += exp
        next = in.readToken()
      } while (next eq separator)
      
      if (next != delim) error(next,"`,' or `)'")
      exps.toArray
    }
  }
  
  private def parseArgs(): Array[AST] = { parseExps(Comma,RightParen) }
  
  /** Parses <id-list> where
    *   <id-list>       ::= <empty> | <prop-id-list>
    *   <prop-id-list>  ::= <id> | <id> , <id-list> 
    *  NOTE: consumes 'to' following <id-list>
    */
  private def parseVars(): Array[Variable] = {
    
//    System.err.println("parseVars called; next token is '" + in.peek())
    
    var vars:ArrayBuffer[Variable] = ArrayBuffer()
    var t:Token = in.readToken()
    if (t eq in.TO) {
//      System.err.println("Encountered empty map parameter list")
      new Array[Variable](0)
    }
    else {
      if (! t.isInstanceOf[Variable]) error(t, "variable")
//     System.err.println("Variable '" + t + "' parsed")
      vars += t.asInstanceOf[Variable]
      t = in.readToken()
      // The absence of "break" in Scala forces ugly code repition here.  Tail recursion on the JVM is a crock!
      while (t != in.TO) {
        if (t != Comma) error(t, "`to' or `, '")
        // Comma found, read next variable
        t = in.readToken()
        // repeated code follows
        if (! t.isInstanceOf[Variable]) error(t,"variable")
//       System.err.println("Variable '" + t + "' parsed")
        vars += t.asInstanceOf[Variable]
        t = in.readToken()
      }
      vars.toArray
    }
  }
  
  /** Parses a proper list of definitions, more technically parses
    *   <prop-def-list> ::= <def> | <def> <def-list> 
    *   NOTE: consumes 'in' following <prop-def-list> */
  
  private def parseDefs(forceMap: Boolean): Array[Def] = {
    val defs: ArrayBuffer[Def] = ArrayBuffer()
    var t: Token = in.readToken()
    
    do {
      var d = parseDef(t)        
      if (forceMap && (! (d.rhs.isInstanceOf[MapLiteral])))
        throw new ParseException("right hand side of definition `" + d + "' is not a map expression")
      defs += d
      t = in.readToken()
    } while (t != in.IN)
    
    defs.toArray
  }
  
  /** Parses 
    *   <id> := <exp> ;
    * which is <def> given that first token var has been read.
    */
  private def parseDef(t: Token): Def = {
    
    if (! (t.isInstanceOf[Variable]))  error(t, "variable")
    
    val key = in.readToken()
    if (key != in.BIND) error (key,"`:='")
    
    val exp = parseExp()
    
    val semi = in.readToken()
    if (semi != SemiColon) error(semi,"`;'")
    Def(t.asInstanceOf[Variable], exp)
  }
  
  private def error(found: Token, expected: String): Null = {
    throw new ParseException("Token `" + found + "' appears where " + expected + " was expected")
  }
}

