import munit.FunSuite

import be.adamv.macroloop.collection.MatrixMNArray


object TestMatrices:
  val g23 = MatrixMNArray((1, 3, 6), (1, 2, 4))

  val g22 = MatrixMNArray((4, 10), (16, 20))

  val g44 = g22.flatMap(i => MatrixMNArray((i - 3, i - 2), (i - 1, i)))

  val m1 = MatrixMNArray((2, -3, 4), (53, 3, 5))
  val m2 = MatrixMNArray((3, 3), (5, 0), (-3, 4))
  val m3 = MatrixMNArray((-21, 22), (159, 179))

  val k1 = MatrixMNArray((1, 2), (3, 4))
  val k2 = MatrixMNArray((0, 5), (6, 7))
  val k3 = MatrixMNArray(
    (0, 5, 0, 10),
    (6, 7, 12, 14),
    (0, 15, 0, 20),
    (18, 21, 24, 28))
  val k4 = MatrixMNArray((0, 10), (18, 28))
  val k3tiled: MatrixMNArray[2, 2, MatrixMNArray[2, 2, Int]] = MatrixMNArray(
    (MatrixMNArray((0, 5), (6, 7)),
     MatrixMNArray((0, 10), (12, 14))),
    (MatrixMNArray((0, 15), (18, 21)),
     MatrixMNArray((0, 20), (24, 28))),
  )

  val s1 = MatrixMNArray.from[3, 3, -1 | 0 | 1](Seq(1, -1, 0, -1, 0, 1, 0, 1, -1))
  val s2 = MatrixMNArray.from[3, 3, -1 | 0 | 1](Seq(-1, 0, 0, 0, 0, 0, 0, 0, 1))


class MatrixMNArrayBasicExample extends FunSuite:
  import TestMatrices.*

  test("directional") {
    enum Pos:
      case TL, TR, BL, BR

    assertEquals(g44.convolve(
      MatrixMNArray.from[2, 2, Pos](Pos.values),
      (i, p) => Seq(p -> i), _ ++ _, Nil
    ).slice[1, 4, 1, 4].show,
    """List((TL,1), (TR,2), (BL,3), (BR,4)),List((TL,2), (TR,7), (BL,4), (BR,9)),List((TL,7), (TR,8), (BL,9), (BR,10))
      |List((TL,3), (TR,4), (BL,13), (BR,14)),List((TL,4), (TR,9), (BL,14), (BR,17)),List((TL,9), (TR,10), (BL,17), (BR,18))
      |List((TL,13), (TR,14), (BL,15), (BR,16)),List((TL,14), (TR,17), (BL,16), (BR,19)),List((TL,17), (TR,18), (BL,19), (BR,20))""".stripMargin)
  }

  test("numerical") {
    val g44f = g44.map(_.toFloat)
    assertEquals(g44f.convolve(
      MatrixMNArray((0.25f, 0.25f), (0.25f, 0.25f)),
      _ * _, _ + _, 0f
    ).show,
    """0.25,0.75,2.25,3.75
      |1.0,2.5,5.5,8.5
      |4.0,8.5,11.0,13.5
      |7.0,14.5,16.5,18.5""".stripMargin)
  }

  test("construct") {
    assertEquals(MatrixMNArray
      .tabulate[3, 4, Int]((i, j) => i * 10 + j)
      .flatMap(i => MatrixMNArray(Tuple1((i, i * 2))))
      .show,
    """0,0,1,2,2,4,3,6
      |10,20,11,22,12,24,13,26
      |20,40,21,42,22,44,23,46""".stripMargin)
  }


class MatrixMNArrayBasic extends FunSuite:
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

//  test("filterIndices slice") {
//    assert(g23.filterIndices((_, j) => j < 2) sameElements g23.slice[0, 2, 0, 2].data)
//  }

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

  test("tiled") {
    assert(k3.tiled[2, 2] == k3tiled)
  }

  test("rowsIt toSeqSeq") {
    assert(g23.rowsIt.map(_.toSeq).toSeq == g23.toSeqSeq)
  }

  test("columnsIt transpose toSeqSeq") {
    assert(g23.transpose.columnsIt.map(_.toSeq).toSeq == g23.toSeqSeq)
  }

  test("existsItem forallItem") {
    assert(g23.forallItem((i, j, v) => (if j == 0 then 1 else Seq(3, 2)(i)*j) == v))
    assert(m1.existsItem((i, j, v) => i == 0 && v < 0))
    assert(g44.forallItem((i, j, v) => g44(i, j) == v))
    assert(!g44.existsItem((i, j, v) => g44(i, j) != v))
  }

  test("indices containsPosition") {
    assert(g23.indices.forall(g23.containsPosition(_, _)))
  }
