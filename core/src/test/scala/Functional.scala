import munit.FunSuite

import macroloop.*

class Functional extends FunSuite:
  test("java array") {
    val a = Array[java.lang.Integer](1, 2, 3)

//    val f = new Function[java.lang.Integer, java.lang.Void]:
//      override def apply(t: java.lang.Integer) = println(t)
//    println(a.sum(using Numeric.IntIsIntegral.asInstanceOf))
    //    ArrayIndex.forEach[Int](a)(println)
//    port.ArrayIndex.forEach[Int, Unit](a.asInstanceOf, println)
  }

  test("arg tuple".only) {
    //    println(show(ConstantTuple.mapPFCompiletime((1, "test", null))({
//      case null => None
//      case x => Some(x)
//    })))

//    println(show(ConstantTuple.mapUnrolledCompiletime((1, "test", null))(_.toString)))
  }
