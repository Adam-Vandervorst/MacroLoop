import munit.FunSuite

import be.adamv.macroloop.collection.*

class CrossBasic extends FunSuite:
  import TestVectors.*
  import TestMatrices.*

  test("inner row column singleElement") {
    assert(v1.asRow.multiply(v2.asColumn, _ * _, _ + _, 0).singleElement == (2 * 4 + 3 * 5))
  }

  test("outer reshape") {
    assert(v1.outer(v2, _ * _) == v3.reshape[2, 2])
  }

  test("inner row column") {
    assert(v1.asColumn.multiply(v2.asRow, _ * _, _ + _, 0) == v3.reshape[2, 2])
  }

  test("diagonal") {
    assert(k3.diagonal == v5)
  }
