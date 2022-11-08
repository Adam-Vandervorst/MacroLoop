import be.adamv.macroloop.collection.ArraySizedVector
import munit.FunSuite


object TestVectors:
  val v1 = ArraySizedVector(2, 3)
  val v2 = ArraySizedVector(4, 5)
  val v3 = ArraySizedVector(2 * 4, 2 * 5, 3 * 4, 3 * 5)
  val v4 = ArraySizedVector(2 * 4, 3 * 5)
  val v5 = ArraySizedVector(0, 7, 0, 28)


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
