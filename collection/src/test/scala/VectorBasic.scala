import munit.FunSuite

import macroloop.collection.*


object TestVectors:
  val v1 = SizedVector.from[2, Int](Seq(2, 3))
  val v2 = SizedVector.from[2, Int](Seq(4, 5))
  val v3 = SizedVector.from[4, Int](Seq(2 * 4, 2 * 5, 3 * 4, 3 * 5))
  val v4 = SizedVector.from[2, Int](Seq(2 * 4, 3 * 5))

class VectorBasic extends FunSuite:
  import TestVectors.*

  test("inner") {
    assert(v1.inner(v2, _ * _, _ + _, 0) == (2*4 + 3*5))
  }

  test("kronecker") {
    assert(v1.kronecker(v2, _ * _) == v3)
  }

  test("hadamard") {
    assert(v1.hadamard(v2, _ * _) == v4)
  }
