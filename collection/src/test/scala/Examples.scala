import munit.FunSuite

import macroloop.collection.*

class Example extends FunSuite:
  val g23 = Grid.from[2, 3, Int](Seq(1, 3, 6, 1, 2, 4))

  val g22 = Grid.from[2, 2, Int](Seq(4, 10, 16, 20))

  val g44 = g22.flatMap(i => Grid.from[2, 2, Int](Seq(i - 3, i - 2, i - 1, i)))

  test("directional") {
    enum Pos { case TL, TR, BL, BR }
    println(g44.convolve(
      Grid.from[2, 2, Pos](Pos.values),
      (i, p) => Seq(p -> i), _ ++ _, Nil
    ).slice[1, 4, 1, 4].show)
  }

  test("numerical") {
    val g44f = g44.map(_.toFloat)
    println(g44f.convolve(
      Grid.from[2, 2, Float](Seq(0.25f, 0.25f, 0.25f, 0.25f)),
      _ * _, _ + _, 0f
    ).show)
  }

  test("construct") {
    println(Grid.tabulate[3, 4, Int]((i, j) => i*10 + j).flatMap(i => Grid.from[1, 2, Int](Seq(i, i*2))).show)
  }


