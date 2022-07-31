package macroloop.collection

import scala.reflect.ClassTag
import compiletime.constValue
import compiletime.ops.int.*

// row-major    rows      columns
class Grid[M <: Int, N <: Int, A : ClassTag](val data: Array[A]):
  inline def apply(inline i: Int, inline j: Int): A = data(i*constValue[N] + j)
  inline def update(pos: (Int, Int), v: A): Unit = data(pos._1*constValue[N] + pos._2) = v

  inline def rows: Iterator[Array[A]] = data.grouped(constValue[N])

  inline def slice[I1 <: Int, I2 <: Int, J1 <: Int, J2 <: Int]: Grid[I2 - I1, J2 - J1, A] =
    Grid.tabulate((i, j) => this(constValue[I1] + i, constValue[J1] + j))

  inline def map[B : ClassTag](inline f: A => B): Grid[M, N, B] = Grid(data.map(f))
  inline def flatMap[O <: Int, P <: Int, B : ClassTag](inline f: A => Grid[O, P, B]): Grid[M*O, N*P, B] = map(f).flatten

  inline def convolve[O <: Int, P <: Int, B, C : ClassTag](kernel: Grid[O, P, B],
      inline combine: (A, B) => C, inline add: (C, C) => C, inline zero: C): Grid[M, N, C] =
    val cx = constValue[O] / 2
    val cy = constValue[P] / 2

    val result = Grid.fill[M, N, C](zero)

    for i <- 0 until constValue[M]
        j <- 0 until constValue[N]
        m <- 0 until constValue[O]
        n <- 0 until constValue[O] do
      val ii = i + (m - cx)
      val jj = j + (n - cy)

      if ii >= 0 && ii < constValue[M] && jj >= 0 && jj < constValue[N] then
        result((i, j)) = add(result(i, j), combine(this(ii, jj), kernel(m, n)))

    result

  inline def show: String = rows.map(row => row.mkString(",")).mkString("\n")

extension [M <: Int, N <: Int, O <: Int, P <: Int, A : ClassTag](nested: Grid[M, N, Grid[O, P, A]])
  inline def flatten: Grid[M*O, N*P, A] =
    val o = constValue[O]; val p = constValue[P]
    Grid.tabulate[M*O, N*P, A]((i, j) => nested(i/o, j/p)(i%o, j%p))

extension [M <: Int, N <: Int, A : ClassTag](g: Grid[M, N, A])(using n: Fractional[A])
  inline def average: A = n.div(g.data.fold(n.zero)(n.plus), n.fromInt(g.data.length))

extension [M <: Int, N <: Int, A : ClassTag](g: Grid[M, N, A])(using n: Ordering[A])
  inline def median: A = g.data.sorted(using n)(g.data.length/2)


object Grid:
  inline def from[M <: Int, N <: Int, A : ClassTag](as: IterableOnce[A]): Grid[M, N, A] = Grid(Array.from(as))

  inline def tabulate[M <: Int, N <: Int, A : ClassTag](inline f: (Int, Int) => A): Grid[M, N, A] =
    val m = constValue[M]
    val n = constValue[N]
    val data = new Array[A](n*m)
    var j = 0
    while j < m do
      var i = 0
      while i < n do
        data(j*n + i) = f(j, i)
        i += 1
      j += 1
    Grid(data)

  inline def fill[M <: Int, N <: Int, A : ClassTag](v: A): Grid[M, N, A] = Grid.tabulate((_, _) => v)
