import munit.FunSuite

import macroloop.collection.*

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

class MatrixBasic extends FunSuite:
  import TestMatrices.*

  test("directional") {
    enum Pos { case TL, TR, BL, BR }
    println(g44.convolve(
      Matrix.from[2, 2, Pos](Pos.values),
      (i, p) => Seq(p -> i), _ ++ _, Nil
    ).slice[1, 4, 1, 4].show)
  }

  test("numerical") {
    val g44f = g44.map(_.toFloat)
    println(g44f.convolve(
      Matrix.from[2, 2, Float](Seq(0.25f, 0.25f, 0.25f, 0.25f)),
      _ * _, _ + _, 0f
    ).show)
  }

  test("construct") {
    println(Matrix.tabulate[3, 4, Int]((i, j) => i*10 + j).flatMap(i => Matrix.from[1, 2, Int](Seq(i, i*2))).show)
  }

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