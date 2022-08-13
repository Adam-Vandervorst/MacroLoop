import munit.FunSuite

import be.adamv.macroloop.collection.Matrix


object TestMatrices:
  val g23 = Matrix.from[2, 3, Int](Seq(1, 3, 6, 1, 2, 4))

  val g22 = Matrix.from[2, 2, Int](Seq(4, 10, 16, 20))

  val g44 = g22.flatMap(i => Matrix.from[2, 2, Int](Seq(i - 3, i - 2, i - 1, i)))

  val m1 = Matrix.from[2, 3, Int](Seq(2, -3, 4, 53, 3, 5))
  val m2 = Matrix.from[3, 2, Int](Seq(3, 3, 5, 0, -3, 4))
  val m3 = Matrix.from[2, 2, Int](Seq(-21, 22, 159, 179))

  val k1 = Matrix.from[2, 2, Int](Seq(1, 2, 3, 4))
  val k2 = Matrix.from[2, 2, Int](Seq(0, 5, 6, 7))
  val k3 = Matrix.from[4, 4, Int](Seq(0, 5, 0, 10,
    6, 7, 12, 14,
    0, 15, 0, 20,
    18, 21, 24, 28))
  val k4 = Matrix.from[2, 2, Int](Seq(0, 10, 18, 28))

  val s1 = Matrix.from[3, 3, -1 | 0 | 1](Seq(1, -1, 0, -1, 0, 1, 0, 1, -1))
  val s2 = Matrix.from[3, 3, -1 | 0 | 1](Seq(-1, 0, 0, 0, 0, 0, 0, 0, 1))


class MatrixBasicExample extends FunSuite:
  import TestMatrices.*

  test("directional") {
    enum Pos:
      case TL, TR, BL, BR

    assertEquals(g44.convolve(
      Matrix.from[2, 2, Pos](Pos.values),
      (i, p) => Seq(p -> i), _ ++ _, Nil
    ).slice[1, 4, 1, 4].show,
    """List((TL,1), (TR,2), (BL,3), (BR,4)),List((TL,2), (TR,7), (BL,4), (BR,9)),List((TL,7), (TR,8), (BL,9), (BR,10))
      |List((TL,3), (TR,4), (BL,13), (BR,14)),List((TL,4), (TR,9), (BL,14), (BR,17)),List((TL,9), (TR,10), (BL,17), (BR,18))
      |List((TL,13), (TR,14), (BL,15), (BR,16)),List((TL,14), (TR,17), (BL,16), (BR,19)),List((TL,17), (TR,18), (BL,19), (BR,20))""".stripMargin)
  }

  test("numerical") {
    val g44f = g44.map(_.toFloat)
    assertEquals(g44f.convolve(
      Matrix.from[2, 2, Float](Seq(0.25f, 0.25f, 0.25f, 0.25f)),
      _ * _, _ + _, 0f
    ).show,
    """0.25,0.75,2.25,3.75
      |1.0,2.5,5.5,8.5
      |4.0,8.5,11.0,13.5
      |7.0,14.5,16.5,18.5""".stripMargin)
  }

  test("construct") {
    assertEquals(Matrix
      .tabulate[3, 4, Int]((i, j) => i * 10 + j)
      .flatMap(i => Matrix.from[1, 2, Int](Seq(i, i * 2)))
      .show,
    """0,0,1,2,2,4,3,6
      |10,20,11,22,12,24,13,26
      |20,40,21,42,22,44,23,46""".stripMargin)
  }


class MatrixBasic extends FunSuite:
  import TestMatrices.*

  test("mm") {
    assert(m1.multiply(m2, _ * _, _ + _, 0) == m3)
  }

  test("kronecker") {
    assert(k1.kronecker(k2, _ * _) == k3)
  }

  test("hadamard") {
    assert(k1.hadamard(k2, _ * _) == k4)
  }

  test("frobenius") {
    assert(k1.frobenius(k2, _ * _, _ + _, 0) == k1.hadamard(k2, _ * _).data.sum)
  }

  test("filterIndices slice") {
    assert(g23.filterIndices((_, j) => j < 2) sameElements g23.slice[0, 2, 0, 2].data)
  }

  test("isSquare") {
    assert(g22.isSquare)
    assert(!g23.isSquare)
  }

  test("isSymmetric") {
    assert(s1.isSymmetric)
    assert(s2.isSymmetric)
    assert(!k1.isSymmetric)
    assert(!k2.isSymmetric)
  }