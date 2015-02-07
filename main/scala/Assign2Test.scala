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
  def testBinAdd() = {
    new Interpreter(new StringReader("1+2")).callByValue match {
      case IntConstant(3) =>
      case _ => fail("Not the correct sum")
    }
  }
}
