import java.io.StringReader
import java.text.ParseException

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
    assertEquals(answer, interp.callByValue.toString())
  }

  def nameCheck(answer: String, program: String) = {
    var interp = new Interpreter(new StringReader(program))
    assertEquals(answer, interp.callByName.toString())
  }

  def needCheck(answer: String, program: String) = {
    var interp = new Interpreter(new StringReader(program))
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
      case _ => fail("numberP threw ");
    }
  } //end of func

  def testMathOP() {
    try {
      var output = "18";
      var input = "2 * 3 + 12";
      allCheck(output, input );

    } catch{
      case _ => fail("MathOP threw ");
    }
  } //end of func

  def testParseException() {
    try {
      var output = "haha";
      var input = " 1 +"
      allCheck(output, input);
      fail("parseException did not throw ParseException exception");
    }catch{
      case _ =>
    }
  } //end of func

  def testEvalException() {
    try {
      var output = "mojo";
      var input = "1 + number?"
      allCheck(output, input);
      fail("parseException did not throw ParseException exception");
    }catch{
      case _ =>
    }
  } //end of func

  def testOutput() {
    try {
      var output = "(1 2)";
      var input = "cons(1, cons(2, null))";
      allCheck(output, input );

    } catch{
      case _ => fail("numberP threw ");
    }
  } //end of func

  def testOutput2() {
    try {
      var output = "true";
      var input = "null?(null)";

      allCheck(output, input );
    } catch{
      case _ => fail("numberP threw ");
    }
  } //end of func

  def testOutput3() {
    try {
      var output = "true";
      var input = "cons?(cons(1, null))";

      allCheck(output, input );
    } catch{
      case _ => fail("numberP threw ");
    }
  } //end of func
}