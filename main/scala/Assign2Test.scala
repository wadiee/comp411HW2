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

  def testhard1() = {
    assert(new Interpreter(new StringReader("let Y    := map f to\n      let g := map x to f(map z to (x(x))(z));\n    in g(g);\n    FACT := map f to\n    map n to if n = 0 then 1 else n * f(n - 1);\n    in (Y(FACT))(3)")).callByValue.toString.equals("6"))
  }


}