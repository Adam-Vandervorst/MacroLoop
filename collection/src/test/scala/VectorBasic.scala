import be.adamv.macroloop.collection.SizedVector
import munit.FunSuite


object TestVectors:
  val v1 = SizedVector(2, 3)
  val v2 = SizedVector(4, 5)
  val v3 = SizedVector(2 * 4, 2 * 5, 3 * 4, 3 * 5)
  val v4 = SizedVector(2 * 4, 3 * 5)
  val v5 = SizedVector(0, 7, 0, 28)


class VectorBasic extends FunSuite:
  import TestVectors.*

  test("inner") {
    assert(v1.inner(v2, _ * _, _ + _, 0) == (2*4 + 3*5))
  }

  test("kronecker") {
    assert(v1.kronecker(v2, _ * _) == v3)
  }

  test("hadamard") {
    assert(v1.elementwise(v2, _ * _) == v4)
  }

  test("destruct") {
//    println(SizedVector.unapply(v1)) // works fine
    val SizedVector(a, b) = v1
    println(s"a: $a, b: $b")
  }
