/** file Interpreter.scala **/
class EvalException(msg: String) extends RuntimeException(msg)

class Interpreter(reader: java.io.Reader) {
  def this(fileName: String) = this(new java.io.FileReader(fileName))

  val ast: AST = new Parser(reader).parse()

  def callByValue: JamVal = {
    def binIntOp(arg1: AST, arg2: AST, op: (Int, Int) => Int, exceptionContent: String) = (helper(arg1), helper(arg2)) match {
      case (IntConstant(value1: Int), IntConstant(value2: Int)) => IntConstant(op(value1, value2))
      case _ => throw new EvalException(exceptionContent)
    }
    def binIntCmpOp(arg1: AST, arg2: AST, op: (Int, Int) => Boolean, exceptionContent: String) = (helper(arg1), helper(arg2)) match {
      case (IntConstant(value1: Int), IntConstant(value2: Int)) => op(value1, value2) match {
        case true => True
        case false => False
      }
      case _ => throw new EvalException(exceptionContent)
    }

    def UnIntOp(arg: AST, op: Int => Int, exceptionContent: String) = helper(arg) match {
      case (IntConstant(value: Int)) => IntConstant(op(value))
      case _ => throw new EvalException(exceptionContent)
    }

    def helper(ast: AST): JamVal = ast match {
      case BinOpApp(binOpPlus: BinOp, arg1: AST, arg2: AST) => binOpPlus match {
        case BinOpPlus => binIntOp(arg1, arg2, _ + _, "BinOpPlus not with int")
        case BinOpMinus => binIntOp(arg1, arg2, _ - _, "BinOpMinus not with int")
        case OpTimes => binIntOp(arg1, arg2, _ * _, "OpTimes not with int")
        case OpDivide => binIntOp(arg1, arg2, _ / _, "OpDivide not with int")
        case OpEquals => ???
        case OpNotEquals => ???
        case OpLessThan => binIntCmpOp(arg1, arg2, _ < _, "BinOpLessthan not with int")
        case OpGreaterThan => binIntCmpOp(arg1, arg2, _ > _, "BinOpGreaterthan not with int")
        case OpLessThanEquals => binIntCmpOp(arg1, arg2, _ <= _, "BinOpLessthanEq not with int")
        case OpGreaterThanEquals => binIntCmpOp(arg1, arg2, _ >= _, "BinOpGreaterthanEq not with int")
        case OpAnd => helper(arg1) match {
          case False => False
          case True => helper(arg2)
        }
        case OpOr => helper(arg1) match {
          case True => True
          case False => helper(arg2)
        }
      }
      // The followings are below Term
      case If(test: AST, conseq: AST, alt: AST) => ???
      case Let(defs: Array[Def], body: AST) => ???
      // Constant
      case bool: BoolConstant => bool
      case EmptyConstant => null     //?? TODO
      case int: IntConstant => int

      case Variable(sym: Symbol) => ???

      case UnOpApp(rator: UnOp, arg: AST) => rator match {
        case UnOpPlus =>  UnIntOp(arg, + _, "unary plus without int")
        case UnOpMinus => UnIntOp(arg, - _, "unary minus without int")
        case OpTilde => helper(ast) match {
          case True => False
          case False => True
        }
      }

      case App(rator: AST, args: Array[AST]) => rator match {
        case MapLiteral(vars: Array[Variable], body: AST) => ???
        // PrimFun
        case FunctionPPrim => ???
        case NumberPPrim => ???
        case ListPPrim => ???
        case ConsPPrim => ???
        case EmptyPPrim => ???
        case ArityPrim => ???
        case ConsPrim => ???
        case FirstPrim => ???
        case RestPrim => ???
      }
    }
    helper(ast)

  }

  def callByName: JamVal = throw new UnsupportedOperationException("Unimplemented")

  def callByNeed: JamVal = throw new UnsupportedOperationException("Unimplemented")


}