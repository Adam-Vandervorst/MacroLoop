import munit.FunSuite

import be.adamv.macroloop.*

class Utility extends FunSuite:
  test("translateCode end-to-end") {
    val inp = List(1, 4, 3)

    assertEquals(show(translateCode({
      val n = 10
      val m = {
        val r = List.range(0, n)
        (r zip r.map(x => x*x)).toMap
      }
      val it = inp.iterator
      while it.hasNext do
        val v = it.next()
        val r = m.getOrElse(v, v*v)
        println(r)
    }, {
      val bufferUpTo = 10
      val mapping = {
        val valueRange = List.range(0, bufferUpTo)
        (valueRange zip valueRange.map(x => x*x)).toMap
      }
      val it = inp.iterator
      while it.hasNext do
        val inpv = it.next()
        val result = mapping.getOrElse(inpv, inpv*inpv)
        println(result)
    })),
    """{
   |  val bufferUpTo: Int = 10
   |  val mapping: Map[Int, Int] = {
   |    val valueRange: List[Int] = List.range[Int](0, bufferUpTo)(IntIsIntegral)
   |    valueRange.zip[Int](valueRange.map[Int](((x: Int) => x.*(x)))).toMap[Int, Int](refl[Tuple2[Int, Int]])
   |  }
   |  val it: Iterator[Int] = inp.iterator
   |  while (it.hasNext) {
   |    val inpv: Int = it.next()
   |    val result: Int = mapping.getOrElse[Int](inpv, inpv.*(inpv))
   |    println(result)
   |  }
   |}""".stripMargin)
  }


