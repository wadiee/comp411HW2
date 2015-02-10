/** file Interpreter.scala **/
case class Environ[V](map: scala.collection.mutable.Map[Symbol, V]) extends Env[V] {
  def this() = this(scala.collection.mutable.Map())
  /** returns the value bound to s in this */
  override def get(s: Symbol): V = map(s)
  /** returns a new environment containing the bindings in this augmented by (s -> v) */
  override def add(p: (Symbol, V)): Env[V] = {
    map += (p._1 -> p._2)
    this
  }
}
case class JamEmpty(override val list: List[JamVal]) extends JamList(list) {
  def this() = this(Nil)
}

class EvalException(msg: String) extends RuntimeException(msg)
class Interpreter(reader: java.io.Reader) {
  def this(fileName: String) = this(new java.io.FileReader(fileName))

  val ast: AST = new Parser(reader).parse()


  def callByValue: JamVal = {
    def binIntOp(e: Env[BoundVal], arg1: AST, arg2: AST, op: (Int, Int) => Int, exceptionContent: String) = (helper(arg1, e), helper(arg2, e)) match {
      case (IntConstant(value1: Int), IntConstant(value2: Int)) => IntConstant(op(value1, value2))
      case _ => throw new EvalException(exceptionContent)
    }
    def binIntCmpOp(e: Env[BoundVal], arg1: AST, arg2: AST, op: (Int, Int) => Boolean, exceptionContent: String) = (helper(arg1, e), helper(arg2, e)) match {
      case (IntConstant(value1: Int), IntConstant(value2: Int)) => op(value1, value2) match {
        case true => True
        case false => False
      }
      case _ => throw new EvalException(exceptionContent)
    }

    def UnIntOp(e: Env[BoundVal], arg: AST, op: Int => Int, exceptionContent: String) = helper(arg, e) match {
      case (IntConstant(value: Int)) => IntConstant(op(value))
      case _ => throw new EvalException(exceptionContent)
    }

    def JamListToList(jamList: JamList): List[JamVal] = jamList match {
      case JamEmpty => Nil
      case _ => jamList.first :: JamListToList(jamList.rest)
    }

    def helper(ast: AST, e: Env[BoundVal]): JamVal = ast match {
      case BinOpApp(binOpPlus: BinOp, arg1: AST, arg2: AST) => binOpPlus match {
        case BinOpPlus => binIntOp(e, arg1, arg2, _ + _, "BinOpPlus not with int")
        case BinOpMinus => binIntOp(e, arg1, arg2, _ - _, "BinOpMinus not with int")
        case OpTimes => binIntOp(e, arg1, arg2, _ * _, "OpTimes not with int")
        case OpDivide => binIntOp(e, arg1, arg2, _ / _, "OpDivide not with int")
        case OpEquals => (helper(arg1, e), helper(arg2, e)) match {
          case (int1: IntConstant, int2: IntConstant) => int1.value == int2.value match {
              case true => True
              case false => False
            }
          case (obj1, obj2) => obj1 == obj2 match {
            case true => True
            case false => False
          }
        }
        case OpNotEquals => (helper(arg1, e), helper(arg2, e)) match {
          case (int1: IntConstant, int2: IntConstant) => int1.value != int2.value match {
            case true => True
            case false => False
          }
          case (obj1, obj2) => obj1 != obj2 match {
            case true => True
            case false => False
          }
        }
        case OpLessThan => binIntCmpOp(e, arg1, arg2, _ < _, "BinOpLessthan not with int")
        case OpGreaterThan => binIntCmpOp(e, arg1, arg2, _ > _, "BinOpGreaterthan not with int")
        case OpLessThanEquals => binIntCmpOp(e, arg1, arg2, _ <= _, "BinOpLessthanEq not with int")
        case OpGreaterThanEquals => binIntCmpOp(e, arg1, arg2, _ >= _, "BinOpGreaterthanEq not with int")
        case OpAnd => helper(arg1, e) match {
          case False => False
          case True => helper(arg2, e)
        }
        case OpOr => helper(arg1, e) match {
          case True => True
          case False => helper(arg2, e)
        }
      }
      // The followings are below Term
      case If(test: AST, conseq: AST, alt: AST) => helper(test, e) match {
        case True => helper(conseq, e)
        case False => helper(alt, e)
      }
      case Let(defs: Array[Def], body: AST) =>
        defs.map(d => (d.lhs.sym, helper(d.rhs, e))).foreach(pair => e.add(pair))
        helper(body, e)


      // Constant
      case EmptyConstant => ???
      case b: BoolConstant => b
      case i: IntConstant => i



      case Variable(sym: Symbol) => e.get(sym).asInstanceOf[JamVal]

      case UnOpApp(rator: UnOp, arg: AST) => rator match {
        case UnOpPlus =>  UnIntOp(e, arg, + _, "unary plus without int")
        case UnOpMinus => UnIntOp(e, arg, - _, "unary minus without int")
        case OpTilde => helper(ast, e) match {
          case True => False
          case False => True
        }
      }
      case map: MapLiteral => JamClosure(map, e)

      case App(rator: AST, args: Array[AST]) => rator match {
        case MapLiteral(mapSth: Array[Variable], toSth: AST) =>
          if (mapSth.length != args.length)
            throw new EvalException("The number of map key to map value does not match")
          /* add new binding to the closure, then pass on */
          mapSth.zip(args).map(pair => (pair._1.sym, helper(pair._2, e)))
            .foreach(pair => e.add(pair))
          helper(toSth, e)

        // PrimFun
        case FunctionPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument")
          args(0) match {
            case _: JamFun => True
            case _ => False
          }

        case NumberPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument for NumberPPrim")
          args(0) match {
            case IntConstant(_) => True
            case _ => False
          }

        case ListPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument for ListPPrim")
          args(0) match {
            case ConsPrim | EmptyConstant => True
            case _ => False
          }

        case ConsPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument for ConsPPrim")
          args(0) match {
            case ConsPrim => True
            case EmptyConstant => False
            case _ => throw new EvalException("ConsPPrim arg0 not ConsPrim nor EmptyConstant")
          }

        case EmptyPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument for EmptyPPrim")
          args(0) match {
            case EmptyConstant => True
            case _ => False
          }

        case ArityPrim =>
          if (args.length != 1) throw new EvalException("Should have one arguments")
          args(0) match {
            case MapLiteral(vars, _) => IntConstant(vars.length)
            case ConsPrim => IntConstant(2)
            case _: PrimFun => IntConstant(1)
            case _ => throw new EvalException("arg0 is not a function")
          }

        case ConsPrim =>
          if (args.length != 2) throw new EvalException("Should have two arguments")
          new JamList(helper(args(0), e)::JamListToList(helper(args(1), e).asInstanceOf[JamList]))

        case FirstPrim =>
          if (args.length != 1) throw new EvalException("Should have one arguments")
          args(0) match {
            case jl: JamList => jl.first
            case _ => throw new EvalException("arg0 is not a jam list")
          }

        case RestPrim =>
          if (args.length != 1) throw new EvalException("Should have one arguments")
          args(0) match {
            case jl: JamList => jl.rest
            case _ => throw new EvalException("arg0 is not a jam list")
          }

      }
      case pf: PrimFun => pf
    }
    helper(ast, new Environ[BoundVal]())

  }

  def callByName: JamVal = throw new UnsupportedOperationException("Unimplemented")

  def callByNeed: JamVal = throw new UnsupportedOperationException("Unimplemented")
}