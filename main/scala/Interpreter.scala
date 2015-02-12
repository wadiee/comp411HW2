/** file Interpreter.scala **/

abstract class Tuple {
  def getJamVal: JamVal
  def getAST: AST
}

class ValueTuple(jamVal: JamVal, ast: AST) extends Tuple{
  override def getJamVal = jamVal
  override def getAST = ast
}

class NameTuple(jamVal: => JamVal, ast: => AST) extends Tuple{
  override def getJamVal = jamVal
  override def getAST = ast
}

class NeedTuple(helper: (AST, Map[Symbol, NeedTuple]) => JamVal, notUntil: (AST, Map[Symbol, NeedTuple]) => AST, env: Map[Symbol, NeedTuple], rawVar: AST) extends Tuple{
  lazy val jamVal = helper(rawVar, env)
  lazy val lazyAst = notUntil(rawVar, env)
  override def getJamVal = jamVal
  override def getAST = lazyAst
}

class EvalException(msg: String) extends RuntimeException(msg)
class Interpreter(reader: java.io.Reader) {
  def this(fileName: String) = this(new java.io.FileReader(fileName))

  val ast: AST = new Parser(reader).parse()

  def callByValue: JamVal = {
    def binIntOp(e: Map[Symbol, ValueTuple], arg1: AST, arg2: AST, op: (Int, Int) => Int, exceptionContent: String) = (helper(arg1, e), helper(arg2, e)) match {
      case (IntConstant(value1: Int), IntConstant(value2: Int)) => IntConstant(op(value1, value2))
      case _ => throw new EvalException(exceptionContent)
    }
    def binIntCmpOp(e: Map[Symbol, ValueTuple], arg1: AST, arg2: AST, op: (Int, Int) => Boolean, exceptionContent: String) = (helper(arg1, e), helper(arg2, e)) match {
      case (IntConstant(value1: Int), IntConstant(value2: Int)) => op(value1, value2) match {
        case true => True
        case false => False
      }
      case _ => throw new EvalException(exceptionContent)
    }

    def UnIntOp(e: Map[Symbol, ValueTuple], arg: AST, op: Int => Int, exceptionContent: String) = helper(arg, e) match {
      case (IntConstant(value: Int)) => IntConstant(op(value))
      case _ => throw new EvalException(exceptionContent)
    }
    def untilNotVariable(input: AST, e: Map[Symbol, ValueTuple]): AST = {
      input match {
        case vv: Variable => untilNotVariable(e(vv.sym).getAST, e)
        case _ => input
      }
    }

    def helper(ast: AST, e: Map[Symbol, ValueTuple]): JamVal = ast match {
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
          case _ => throw new EvalException("Never should be here!")
        }
        case OpOr => helper(arg1, e) match {
          case True => True
          case False => helper(arg2, e)
          case _ => throw new EvalException("Never should be here!")
        }
      }
      // The followings are below Term
      case If(test: AST, conseq: AST, alt: AST) => helper(test, e) match {
        case True => helper(conseq, e)
        case False => helper(alt, e)
        case _ => throw new EvalException("Never should be here!")
      }
      case Let(defs: Array[Def], body: AST) =>
        var newMap = e
        defs.map(d => (d.lhs.sym, new ValueTuple(helper(d.rhs, e), untilNotVariable(d.rhs, e)))).foreach(pair => newMap += pair)
        helper(body, newMap)

      // Constant
      case EmptyConstant => EmptyConstant
      case b: BoolConstant => b
      case i: IntConstant => i

      case Variable(sym: Symbol) => {
        e(sym).getJamVal
      }

      case UnOpApp(rator: UnOp, arg: AST) => rator match {
        case UnOpPlus =>  UnIntOp(e, arg, + _, "unary plus without int")
        case UnOpMinus => UnIntOp(e, arg, - _, "unary minus without int")
        case OpTilde => helper(arg, e) match {
          case True => False
          case False => True
          case _ => throw new EvalException("Never should be here!")
        }
      }
      case map: MapLiteral => JamClosure(map, e)

      case App(rator: AST, args: Array[AST]) => helper(rator, e) match {
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
            case EmptyConstant => True
            case App(ract, _) => ract match {
              case ConsPrim => True
              case _ => False
            }
            case _ => False
          }

        case ConsPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument for ConsPPrim")
          args(0) match {
            case App(ract, _) => ract match {
              case ConsPrim => True
              case _ => False
            }
            case _ => False
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
          new JamListNE(helper(args(0), e), helper(args(1), e).asInstanceOf[JamList])

        case FirstPrim =>
          if (args.length != 1) throw new EvalException("Should have one arguments")
          args(0) match {
            case va: Variable => e(va.sym).getJamVal match {
              case jl: JamListNE => jl.first
              case _ => throw new EvalException("Calling FirstPrim on a non-list variable")
            }
            case App(rator, l) => rator match {
              case ConsPrim => helper(l(0), e)
              case _ => throw new EvalException("should have consprim as the rator")
            }
            case _ => throw new EvalException("arg0 is not a jam list, it is a " + args(0).getClass)
          }

        case RestPrim =>
          if (args.length != 1) throw new EvalException("Should have one arguments")
          args(0) match {
            case va: Variable => e(va.sym).getJamVal match {
              case jl: JamListNE => jl.rest
              case _ => throw new EvalException("Calling RestPrim on a non-list variable")
            }
            case App(rator, l) => rator match {
              case ConsPrim => helper(l(1), e)
              case _ => throw new EvalException("should have consprim as the rator")
            }
            case _ => throw new EvalException("arg0 is not a jam list, it is a " + args(0).getClass)
          }

        case JamClosure(MapLiteral(vars, body), en) => {
          // Bind
          if (vars.length != args.length) throw new EvalException("The length of vars and args are not the same")
          var newMap = en
          vars.zip(args).map(pair => (pair._1.sym, new ValueTuple(helper(pair._2, e), untilNotVariable(pair._2, e)))).
            foreach(pair => newMap += (pair))
          helper(body, newMap)
        }
        case _=> throw new EvalException("Did not match. Got a class: " + rator.getClass)
      }
      case pf: PrimFun => pf
    }
    helper(ast, Map())
  }

  def callByName: JamVal = {
    def binIntOp(e: Map[Symbol, NameTuple], arg1: AST, arg2: AST, op: (Int, Int) => Int, exceptionContent: String) = (helper(arg1, e), helper(arg2, e)) match {
      case (IntConstant(value1: Int), IntConstant(value2: Int)) => IntConstant(op(value1, value2))
      case _ => throw new EvalException(exceptionContent)
    }
    def binIntCmpOp(e: Map[Symbol, NameTuple], arg1: AST, arg2: AST, op: (Int, Int) => Boolean, exceptionContent: String) = (helper(arg1, e), helper(arg2, e)) match {
      case (IntConstant(value1: Int), IntConstant(value2: Int)) => op(value1, value2) match {
        case true => True
        case false => False
      }
      case _ => throw new EvalException(exceptionContent)
    }

    def UnIntOp(e: Map[Symbol, NameTuple], arg: AST, op: Int => Int, exceptionContent: String) = helper(arg, e) match {
      case (IntConstant(value: Int)) => IntConstant(op(value))
      case _ => throw new EvalException(exceptionContent)
    }
    def untilNotVariable(input: AST, e: Map[Symbol, NameTuple]): AST = {
      input match {
        case vv: Variable => untilNotVariable(e(vv.sym).getAST, e)
        case _ => input
      }
    }

    def helper(ast: AST, e: Map[Symbol, NameTuple]): JamVal = ast match {
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
          case _ => throw new EvalException("Never should be here!")
        }
        case OpOr => helper(arg1, e) match {
          case True => True
          case False => helper(arg2, e)
          case _ => throw new EvalException("Never should be here!")
        }
      }
      // The followings are below Term
      case If(test: AST, conseq: AST, alt: AST) => helper(test, e) match {
        case True => helper(conseq, e)
        case False => helper(alt, e)
        case _ => throw new EvalException("Never should be here!")
      }
      case Let(defs: Array[Def], body: AST) =>
        var newMap = e
        defs.map(d => (d.lhs.sym, new NameTuple(helper(d.rhs, e), untilNotVariable(d.rhs, e)))).foreach(pair => newMap += pair)
        helper(body, newMap)

      // Constant
      case EmptyConstant => EmptyConstant
      case b: BoolConstant => b
      case i: IntConstant => i

      case Variable(sym: Symbol) => {
        e(sym).getJamVal
      }

      case UnOpApp(rator: UnOp, arg: AST) => rator match {
        case UnOpPlus =>  UnIntOp(e, arg, + _, "unary plus without int")
        case UnOpMinus => UnIntOp(e, arg, - _, "unary minus without int")
        case OpTilde => helper(arg, e) match {
          case True => False
          case False => True
          case _ => throw new EvalException("Never should be here!")
        }
      }
      case map: MapLiteral => NameJamClosure(map, e)

      case App(rator: AST, args: Array[AST]) => helper(rator, e) match {

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
            case EmptyConstant => True
            case App(ract, _) => ract match {
              case ConsPrim => True
              case _ => False
            }
            case _ => False
          }

        case ConsPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument for ConsPPrim")
          args(0) match {
            case App(ract, _) => ract match {
              case ConsPrim => True
              case _ => False
            }
            case _ => False
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
          new JamListNE(helper(args(0), e), helper(args(1), e).asInstanceOf[JamList])

        case FirstPrim =>
          if (args.length != 1) throw new EvalException("Should have one arguments")
          args(0) match {
            case va: Variable => e(va.sym).getJamVal match {
              case jl: JamListNE => jl.first
              case _ => throw new EvalException("Calling FirstPrim on a non-list variable")
            }
            case App(rator, l) => rator match {
              case ConsPrim => helper(l(0), e)
              case _ => throw new EvalException("should have consprim as the rator")
            }
            case _ => throw new EvalException("arg0 is not a jam list, it is a " + args(0).getClass)
          }

        case RestPrim =>
          if (args.length != 1) throw new EvalException("Should have one arguments")
          args(0) match {
            case va: Variable => e(va.sym).getJamVal match {
              case jl: JamListNE => jl.rest
              case _ => throw new EvalException("Calling RestPrim on a non-list variable")
            }
            case App(rator, l) => rator match {
              case ConsPrim => helper(l(1), e)
              case _ => throw new EvalException("should have consprim as the rator")
            }
            case _ => throw new EvalException("arg0 is not a jam list, it is a " + args(0).getClass)
          }

        case NameJamClosure(MapLiteral(vars, body), en) => {
          // Bind
          if (vars.length != args.length) throw new EvalException("The length of vars and args are not the same")
          var newMap = en
          vars.zip(args).map(pair => (pair._1.sym, new NameTuple(helper(pair._2, e), untilNotVariable(pair._2, e)))).
            foreach(pair => newMap += (pair))
          helper(body, newMap)
        }
        case _=> throw new EvalException("Did not match. Got a class: " + rator.getClass)
      }
      case pf: PrimFun => {
        pf
      }
    }
    helper(ast, Map())
  }

  def callByNeed: JamVal = {
    def binIntOp(e: Map[Symbol, NeedTuple], arg1: AST, arg2: AST, op: (Int, Int) => Int, exceptionContent: String) = (helper(arg1, e), helper(arg2, e)) match {
      case (IntConstant(value1: Int), IntConstant(value2: Int)) => IntConstant(op(value1, value2))
      case _ => throw new EvalException(exceptionContent)
    }
    def binIntCmpOp(e: Map[Symbol, NeedTuple], arg1: AST, arg2: AST, op: (Int, Int) => Boolean, exceptionContent: String) = (helper(arg1, e), helper(arg2, e)) match {
      case (IntConstant(value1: Int), IntConstant(value2: Int)) => op(value1, value2) match {
        case true => True
        case false => False
      }
      case _ => throw new EvalException(exceptionContent)
    }

    def UnIntOp(e: Map[Symbol, NeedTuple], arg: AST, op: Int => Int, exceptionContent: String) = helper(arg, e) match {
      case (IntConstant(value: Int)) => IntConstant(op(value))
      case _ => throw new EvalException(exceptionContent)
    }
    def untilNotVariable(input: AST, e: Map[Symbol, NeedTuple]): AST = {
      input match {
        case vv: Variable => untilNotVariable(e(vv.sym).getAST, e)
        case _ => input
      }
    }

    def helper(ast: AST, e: Map[Symbol, NeedTuple]): JamVal = ast match {
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
          case _ => throw new EvalException("Never should be here!")
        }
        case OpOr => helper(arg1, e) match {
          case True => True
          case False => helper(arg2, e)
          case _ => throw new EvalException("Never should be here!")
        }
      }
      // The followings are below Term
      case If(test: AST, conseq: AST, alt: AST) => helper(test, e) match {
        case True => helper(conseq, e)
        case False => helper(alt, e)
        case _ => throw new EvalException("Never should be here!")
      }
      case Let(defs: Array[Def], body: AST) =>
        var newMap = e
        defs.map(d => (d.lhs.sym, new NeedTuple(helper, untilNotVariable, e, d.rhs))).foreach(pair => newMap += pair)
        helper(body, newMap)

      // Constant
      case EmptyConstant => EmptyConstant
      case b: BoolConstant => b
      case i: IntConstant => i

      case Variable(sym: Symbol) => {
        e(sym).getJamVal
      }

      case UnOpApp(rator: UnOp, arg: AST) => rator match {
        case UnOpPlus =>  UnIntOp(e, arg, + _, "unary plus without int")
        case UnOpMinus => UnIntOp(e, arg, - _, "unary minus without int")
        case OpTilde => helper(arg, e) match {
          case True => False
          case False => True
          case _ => throw new EvalException("Never should be here!")
        }
      }
      case map: MapLiteral => NeedJamClosure(map, e)

      case App(rator: AST, args: Array[AST]) => helper(rator, e) match {
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
            case EmptyConstant => True
            case App(ract, _) => ract match {
              case ConsPrim => True
              case _ => False
            }
            case _ => False
          }

        case ConsPPrim =>
          if (args.length != 1) throw new EvalException("Should have one argument for ConsPPrim")
          args(0) match {
            case App(ract, _) => ract match {
              case ConsPrim => True
              case _ => False
            }
            case _ => False
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
          new JamListNE(helper(args(0), e), helper(args(1), e).asInstanceOf[JamList])

        case FirstPrim =>
          if (args.length != 1) throw new EvalException("Should have one arguments")
          args(0) match {
            case va: Variable => e(va.sym).getJamVal match {
              case jl: JamListNE => jl.first
              case _ => throw new EvalException("Calling FirstPrim on a non-list variable")
            }
            case App(rator, l) => rator match {
              case ConsPrim => helper(l(0), e)
              case _ => throw new EvalException("should have consprim as the rator")
            }
            //case jl: JamListNE => jl.first
            case _ => throw new EvalException("arg0 is not a jam list, it is a " + args(0).getClass)
          }

        case RestPrim =>
          if (args.length != 1) throw new EvalException("Should have one arguments")
          args(0) match {
            case va: Variable => e(va.sym).getJamVal match {
              case jl: JamListNE => jl.rest
              case _ => throw new EvalException("Calling RestPrim on a non-list variable")
            }
            case App(rator, l) => rator match {
              case ConsPrim => helper(l(1), e)
              case _ => throw new EvalException("should have consprim as the rator")
            }
            case _ => throw new EvalException("arg0 is not a jam list, it is a " + args(0).getClass)
          }

        case NeedJamClosure(MapLiteral(vars, body), en) => {
          // Bind
          if (vars.length != args.length) throw new EvalException("The length of vars and args are not the same")
          var newMap = en
          vars.zip(args).map(pair => (pair._1.sym, new NeedTuple(helper, untilNotVariable, e, pair._2))).foreach(pair => newMap += (pair))
          helper(body, newMap)
        }
        case _=> throw new EvalException("Did not match. Got a class: " + rator.getClass)
      }
      case pf: PrimFun => pf
    }
    helper(ast, Map())
  }
}