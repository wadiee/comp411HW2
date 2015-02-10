import java.io.StringReader

import junit.framework.TestCase
//import junit.framework._
import junit.framework.Assert._
//import junit.framework._
//import java.io._;

/**
 * Created by Wade on 2/6/2015.
 */
class Assign2Test extends TestCase{

  def testConsFalse() = {
    new Interpreter(new StringReader("cons(1,cons(2,null))=cons(1,cons(3,null))")).callByValue match {
      case False =>
      case _ => fail("Should False")
    }
  }

  def testBinAdd() = {
    new Interpreter(new StringReader("1+2")).callByValue match {
      case IntConstant(3) =>
      case _ => fail("Not the correct sum")
    }
  }

  def testBinCmp() = {
    new Interpreter(new StringReader("5 = 5")).callByValue match {
      case True =>
      case _ => fail("Should True")
    }
  }

  def testBinCmpFalse() = {
    new Interpreter(new StringReader("5 = 7")).callByValue match {
      case False =>
      case _ => fail("Should False")
    }
  }

  def testConsTrue() = {
    new Interpreter(new StringReader("cons(1,cons(2,null))=cons(1,cons(2,null))")).callByValue match {
      case True =>
      case _ => fail("Should True")
    }
  }


  def testBinConsFalse() = {
    new Interpreter(new StringReader("cons(1,cons(2,null))=cons(1,null)")).callByValue match {
      case False =>
      case _ => fail("Not the correct sum")
    }
  }
  def testBinPrimFunTrue() = {
    new Interpreter(new StringReader("cons? = cons?")).callByValue match {
      case True =>
      case _ => fail("Not the correct sum")
    }
  }
  def testBinPrimFunFalse() = {
    new Interpreter(new StringReader("cons? = null?")).callByValue match {
      case False =>
      case _ => fail("Not the correct sum")
    }
  }
  def testBinPrimMapFalse() = {
    new Interpreter(new StringReader("cons? = (map x to x)")).callByValue match {
      case False =>
      case _ => fail("Not the correct sum")
    }
  }
  def testBinObjFalse() = {
    new Interpreter(new StringReader("(map x to x) = (map x to x)")).callByValue match {
      case False =>
      case _ => fail("Not the correct sum")
    }
  }
  def testLet() = {
    new Interpreter(new StringReader("let m:=(map x to x); in m = m")).callByValue match {
      case True =>
      case _ => fail("Not the correct sum")
    }
  }












}
