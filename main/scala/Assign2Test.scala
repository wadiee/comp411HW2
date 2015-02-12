import java.io.StringReader
import junit.framework.TestCase
import junit.framework.Assert._

/**
 * Created by Wade on 2/6/2015.
 */
class Assign2Test extends TestCase{

  def testConsFalse() = {
    assert(new Interpreter(new StringReader("cons(1,cons(2,null))=cons(1,cons(3,null))")).callByValue.toString.equals("false"), "Should not equal")
  }

  def testBinAdd() = {
    assert(new Interpreter(new StringReader("1+2")).callByValue.toString.equals("3"), "1+2 should equal to 3")
  }

  def testBinCmp() = {
    assert(new Interpreter(new StringReader("5 = 5")).callByValue.toString.equals("true"), "Should equal")
  }

  def testBinCmpFalse() = {
    assert(new Interpreter(new StringReader("5 = 7")).callByValue.toString.equals("false"), "Should not equal")
  }

  def testConsTrue() = {
    assert(new Interpreter(new StringReader("cons(1,cons(2,null))=cons(1,cons(2,null))")).callByValue.toString.equals("true"), "cons should equal cons")
  }

  def testBinConsFalse() = {
    assert(new Interpreter(new StringReader("cons(1,cons(2,null))=cons(1,null)")).callByValue.toString.equals("false"))
  }

  def testBinPrimFunTrue() = {
    assert(new Interpreter(new StringReader("cons? = cons?")).callByValue.toString.equals("true"))
  }

  def testBinPrimFunFalse() = {
    assert(new Interpreter(new StringReader("cons? = null?")).callByValue.toString.equals("false"))
  }

  def testBinPrimMapFalse() = {
    assert(new Interpreter(new StringReader("cons? = (map x to x)")).callByValue.toString.equals("false"))
  }

  def testBinObjFalse() = {
    assert(new Interpreter(new StringReader("(map x to x) = (map x to x)")).callByValue.toString.equals("false"))
  }

  def testValLet() = {
    assert(new Interpreter(new StringReader("let m:=(map x to x); in m = m")).callByValue.toString.equals("true"))
  }

  def testValLet1() ={
    assert(new Interpreter(new StringReader("let m:= 5; in m+1")).callByValue.toString.equals("6"))
  }

  def testCons() ={
    assert(new Interpreter(new StringReader("cons(1, cons(2, cons(3, null)))")).callByValue.toString.equals("(1 2 3)"))
  }

  def testmid1 = {
    assert(new Interpreter(new StringReader("let x:=2; in let y:=-x; in (map t to t * y)(100)")).callByValue.toString.equals("-200"))
  }

  def testhard1() = {
    assert(new Interpreter(new StringReader("let Y    := map f to\n      let g := map x to f(map z to (x(x))(z));\n    in g(g);\n    FACT := map f to\n    map n to if n = 0 then 1 else n * f(n - 1);\n    in (Y(FACT))(3)")).callByValue.toString.equals("6"))
  }

  def testhard2() = {
    assert(new Interpreter(new StringReader("let Y    := map f to \n              let g := map x to f(map z1,z2 to (x(x))(z1,z2));\n\t    in g(g);\n    APPEND := map ap to \n\t        map x,y to \n                  if x = null then y else cons(first(x), ap(rest(x), y));\n    l      := cons(1,cons(2,cons(3,null)));\t\nin (Y(APPEND))(l,l)")).callByValue.toString.equals("(1 2 3 1 2 3)"))
  }

//==============================Call by name====================================
  def testName() = {
    assert(new Interpreter(new StringReader("let m:=(map x to x); in m = m")).callByName.toString.equals("false"))
  }

  def testhard3() = {
    assert(new Interpreter(new StringReader("let Y    := map f to\n      let g := map x to f(map z to (x(x))(z));\n    in g(g);\n    FACT := map f to\n    map n to if n = 0 then 1 else n * f(n - 1);\n    in (Y(FACT))(3)")).callByName.toString.equals("6"))
  }

  def testhard4() = {
    assert(new Interpreter(new StringReader("let Y    := map f to \n              let g := map x to f(map z1,z2 to (x(x))(z1,z2));\n\t    in g(g);\n    APPEND := map ap to \n\t        map x,y to \n                  if x = null then y else cons(first(x), ap(rest(x), y));\n    l      := cons(1,cons(2,cons(3,null)));\t\nin (Y(APPEND))(l,l)")).callByName.toString.equals("(1 2 3 1 2 3)"))
  }
//============================Call by need
  def testNeed() = {
    assert(new Interpreter(new StringReader("let m:=(map x to x); in m = m")).callByNeed.toString.equals("true"))
  }

  def testhard5() = {
    assert(new Interpreter(new StringReader("let Y    := map f to\n      let g := map x to f(map z to (x(x))(z));\n    in g(g);\n    FACT := map f to\n    map n to if n = 0 then 1 else n * f(n - 1);\n    in (Y(FACT))(3)")).callByNeed.toString.equals("6"))
  }

  def testhard6() = {
    assert(new Interpreter(new StringReader("let Y    := map f to \n              let g := map x to f(map z1,z2 to (x(x))(z1,z2));\n\t    in g(g);\n    APPEND := map ap to \n\t        map x,y to \n                  if x = null then y else cons(first(x), ap(rest(x), y));\n    l      := cons(1,cons(2,cons(3,null)));\t\nin (Y(APPEND))(l,l)")).callByNeed.toString.equals("(1 2 3 1 2 3)"))
  }
  // Test All
  def valueCheck(answer: String, program: String) = {
    var interp = new Interpreter(new StringReader(program))
    var ge = interp.callByValue.toString()
    assertEquals(answer, interp.callByValue.toString())
  }

  def nameCheck(answer: String, program: String) = {
    var interp = new Interpreter(new StringReader(program))
    var ge = interp.callByName.toString()
    assertEquals(answer, interp.callByName.toString())
  }

  def needCheck(answer: String, program: String) = {
    var interp = new Interpreter(new StringReader(program))
    var ge = interp.callByNeed.toString()
    assertEquals(answer, interp.callByNeed.toString())
  }

  def allCheck(answer: String, program: String) {
    valueCheck(answer, program);
    nameCheck(answer, program);
    needCheck(answer, program);
  }

  def testNumberP() {
    try {
      var output = "number?";
      var input = "number?";
      allCheck(output, input );

    } catch{
      case _: Throwable => fail("numberP threw ");
    }
  } //end of func

  def testMathOP() {
    try {
      var output = "18";
      var input = "2 * 3 + 12";
      allCheck(output, input );

    } catch{
      case _: Throwable => fail("MathOP threw ");
    }
  } //end of func

  def testParseException() {
    try {
      var output = "haha";
      var input = " 1 +"
      allCheck(output, input);
      fail("parseException did not throw ParseException exception");
    }catch{
      case _: Throwable =>
    }
  } //end of func

  def testEvalException() {
    try {
      var output = "mojo";
      var input = "1 + number?"
      allCheck(output, input);
      fail("parseException did not throw ParseException exception");
    }catch{
      case _: Throwable =>
    }
  } //end of func

  // Test primitive functions
  def testOutputCons() {
    try {
      var output = "(1 2)";
      var input = "cons(1, cons(2, null))";
      allCheck(output, input );

    } catch{
      case _: Throwable => fail("cons threw ");
    }
  } //end of func

  def testOutputEmpty() {
    try {
      var output = "true";
      var input = "null?(null)";

      allCheck(output, input );
    } catch{
      case _: Throwable => fail("null? threw ");
    }
  } //end of func

  def testOutputConsP() {
    try {
      var output = "true";
      var input = "cons?(cons(1, null))";

      allCheck(output, input );
    } catch{
      case _: Throwable => fail("cons? threw ");
    }
  } //end of func

  def testOutputNumber() {
    try {
      var output = "true";
      var input = "number?(1)";

      allCheck(output, input );
    } catch{
      case _: Throwable => fail("number? threw ");
    }
  } //end of func

  def testOutputFunction() {
    try {
      var output = "true";
      var input = "function?(number?)";

      allCheck(output, input );
    } catch{
      case _: Throwable => fail("function? threw ");
    }
  } //end of func

  def testOutputArity() {
    try {
      var output = "3";
      var input = "arity(map x, y, z to x)";

      allCheck(output, input );
    } catch{
      case e: Throwable => fail("arity threw " + e.printStackTrace());
    }
  } //end of func

  def testOutputListNull() {
    try {
      var output = "true";
      var input = "list?(null)";

      allCheck(output, input );
    } catch{
      case _: Throwable => fail("list? null threw ");
    }
  } //end of func

  def testOutputList() {
    try {
      var output = "true";
      var input = "list?(cons(1, cons(2, null)))";

      allCheck(output, input );
    } catch{
      case e: Throwable => fail("list? threw " + e.printStackTrace());
    }
  } //end of func

  def testOutputFirst() {
    try {
      var output = "1";
      var input = "first(cons(1, cons(2, null)))";

      allCheck(output, input );
    } catch{
      case e: Throwable => fail("first threw " + e.printStackTrace());
    }
  } //end of func

  def testOutputRest() {
    try {
      var output = "(2)";
      var input = "rest(cons(1, cons(2, null)))";

      allCheck(output, input );
    } catch{
      case e: Throwable => fail("rest threw " + e.printStackTrace());
    }
  } //end of func

  // should fail cases

  def testFailCons() {
    try {
      var output = "false";
      var input = "cons?(map x to 1)";

      allCheck(output, input );
    } catch{
      case e: Throwable => fail("failcons threw " + e.printStackTrace());
    }
  } //end of func

  def testFailNull() {
    try {
      var output = "false";
      var input = "null?(3)";

      allCheck(output, input );
    } catch{
      case e: Throwable => fail("failnull threw " + e.printStackTrace());
    }
  } //end of func

  def testFailNumber() {
    try {
      var output = "false";
      var input = "number?(null)";

      allCheck(output, input );
    } catch{
      case e: Throwable => fail("failnumber threw " + e.printStackTrace());
    }
  } //end of func

  def testFailFunction() {
    try {
      var output = "false";
      var input = "function?(null)";

      allCheck(output, input );
    } catch{
      case e: Throwable => fail("failfunction threw " + e.printStackTrace());
    }
  } //end of func

  def testFaillist() {
    try {
      var output = "false";
      var input = "list?(map x to 1)";

      allCheck(output, input );
    } catch{
      case e: Throwable => fail("faillist threw " + e.printStackTrace());
    }
  } //end of func



  // Test Free variable, should get different result for value, name, need calls.

  def testFreeVarValue() {
    try {
      var output = "5";
      var input = "let x := f; \n in 5";

      try{
        valueCheck(output, input)
        fail("Did not throw free variable")
      } catch {
        case e: Throwable =>
      }
    } catch{
      case e: Throwable => fail("free variable value call by value");
    }
  } //end of func

  def testFreeVarName() {
    try {
      var output = "5";
      var input = "let x := f; \n in 5";

      nameCheck(output, input)
    } catch{
      case e: Throwable => fail("free variable throw for call by name");
    }
  } //end of func

  def testFreeVarNeed() {
    try {
      var output = "5";
      var input = "let x := f; \n in 5";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("free variable throw for call by need");
    }
  } //end of func

  def testUnary() {
    try {
      var output = "5";
      var input = "+5";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("unary throw");
    }
  } //end of func

  def testUnary2() {
    try {
      var output = "-5";
      var input = "-5";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("unary throw");
    }
  } //end of func

  def testUnary3() {
    try {
      var output = "false";
      var input = "~true";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("unary throw");
    }
  } //end of func

  def testUnary4() {
    try {
      var output = "true";
      var input = "~false";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("unary throw");
    }
  } //end of func

  def testBinary1() {
    try {
      var output = "true";
      var input = "5 > 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary2() {
    try {
      var output = "false";
      var input = "5 < 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary3() {
    try {
      var output = "false";
      var input = "5 <= 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary4() {
    try {
      var output = "true";
      var input = "2 <= 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary5() {
    try {
      var output = "true";
      var input = "2 != 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary6() {
    try {
      var output = "false";
      var input = "3 != 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary7() {
    try {
      var output = "5";
      var input = "2 + 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary8() {
    try {
      var output = "-1";
      var input = "2 - 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary9() {
    try {
      var output = "4";
      var input = "8 / 2";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary10() {
    try {
      var output = "6";
      var input = "2 * 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary11() {
    try {
      var output = "true";
      var input = "3 >= 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary12() {
    try {
      var output = "false";
      var input = "2 >= 3";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary13() {
    try {
      var output = "true";
      var input = "true & true";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary14() {
    try {
      var output = "false";
      var input = "false & true";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary15() {
    try {
      var output = "true";
      var input = "false | true";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary16() {
    try {
      var output = "true";
      var input = "true | false";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func

  def testBinary17() {
    try {
      var output = "false";
      var input = "true & false";

      needCheck(output, input)
    } catch{
      case e: Throwable => fail("Binary throw");
    }
  } //end of func
}