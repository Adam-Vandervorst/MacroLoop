package macroloop.collection

import scala.reflect.ClassTag
import compiletime.constValue
import compiletime.ops.int.*


class Matrix[M <: Int, N <: Int, A : ClassTag](val data: Array[A]):
  inline def nrows: M = constValue
  inline def ncolumns: N = constValue
  inline def nitems: M*N = constValue

  inline def apply(inline i: Int, inline j: Int): A = data(i*ncolumns + j)
  inline def update(inline i: Int, inline j: Int, inline v: A): Unit = data(i*ncolumns + j) = v

  inline def rows: Iterator[Array[A]] = data.grouped(ncolumns)

  inline def filterIndices(inline p: (Int, Int) => Boolean): Array[A] =
    val data = Array.newBuilder
    var j = 0
    while j < nrows do
      var i = 0
      while i < ncolumns do
        if p(j, i) then
          data += this(j, i)
        i += 1
      j += 1
    data.result()

  inline def transpose: Matrix[N, M, A] = Matrix.tabulate((i, j) => this(j, i))
  inline def slice[I1 <: Int, I2 <: Int, J1 <: Int, J2 <: Int]: Matrix[I2 - I1, J2 - J1, A] =
    Matrix.tabulate((i, j) => this(constValue[I1] + i, constValue[J1] + j))

  inline def map[B : ClassTag](inline f: A => B): Matrix[M, N, B] = Matrix(data.map(f))
  inline def flatMap[O <: Int, P <: Int, B : ClassTag](inline f: A => Matrix[O, P, B]): Matrix[M*O, N*P, B] = map(f).flatten

  inline def convolve[O <: Int, P <: Int, B, C : ClassTag](kernel: Matrix[O, P, B],
      inline combine: (A, B) => C, inline add: (C, C) => C, inline zero: C): Matrix[M, N, C] =
    val cx = kernel.nrows/2
    val cy = kernel.ncolumns/2

    val result = Matrix.fill[M, N, C](zero)

    for i <- 0 until nrows
        j <- 0 until ncolumns
        m <- 0 until kernel.nrows
        n <- 0 until kernel.ncolumns do
      val ii = i + (m - cx)
      val jj = j + (n - cy)

      if ii >= 0 && ii < nrows && jj >= 0 && jj < ncolumns then
        result(i, j) = add(result(i, j), combine(this(ii, jj), kernel(m, n)))
    result

  inline def multiply[O <: Int, B, C : ClassTag](that: Matrix[N, O, B],
      inline combine: (A, B) => C, inline add: (C, C) => C, inline zero: C): Matrix[M, O, C] =
    val result = Matrix.fill[M, O, C](zero)

    for i <- 0 until this.nrows
        j <- 0 until that.ncolumns
        k <- 0 until this.ncolumns do
      result(i, j) = add(result(i, j), combine(this(i, k), that(k, j)))
    result

  inline def kronecker[O <: Int, P <: Int, B, C : ClassTag](that: Matrix[O, P, B],
      inline combine: (A, B) => C): Matrix[M*O, N*P, C] =
    this.flatMap(a => that.map(b => combine(a, b)))

  inline def hadamard[B, C : ClassTag](that: Matrix[M, N, B],
      inline combine: (A, B) => C): Matrix[M, N, C] =
    val cdata = new Array[C](nitems)
    var i = 0
    while i < nitems do
      cdata(i) = combine(this.data(i), that.data(i))
      i += 1
    Matrix(cdata)

  inline def frobenius[B, C](that: Matrix[M, N, B],
      inline combine: (A, B) => C, inline add: (C, C) => C, inline zero: C): C =
    var c = zero
    var i = 0
    while i < nitems do
      c = add(c, combine(this.data(i), that.data(i)))
      i += 1
    c

  inline def show: String = rows.map(row => row.mkString(",")).mkString("\n")

  override def equals(that: Any): Boolean = that match
    case that: Matrix[m, n, _] => this.data sameElements that.data
    case _ => false

extension [M <: Int, N <: Int](m: Matrix[M, N, _])
  inline def isSquare: Boolean = m.nrows == m.ncolumns

extension [N <: Int, A : ClassTag](m: Matrix[N, N, A])
  inline def diagonal: SizedVector[N, A] = SizedVector(m.filterIndices(_ == _))
  inline def isSymmetric: Boolean = m == m.transpose

extension [M <: Int, N <: Int, O <: Int, P <: Int, A : ClassTag](nested: Matrix[M, N, Matrix[O, P, A]])
  inline def flatten: Matrix[M*O, N*P, A] =
    val o = constValue[O]; val p = constValue[P]
    Matrix.tabulate[M*O, N*P, A]((i, j) => nested(i/o, j/p)(i%o, j%p))

extension [M <: Int, N <: Int, A : ClassTag](g: Matrix[M, N, A])(using n: Fractional[A])
  inline def average: A = n.div(g.data.fold(n.zero)(n.plus), n.fromInt(g.data.length))

extension [M <: Int, N <: Int, A : ClassTag](g: Matrix[M, N, A])(using n: Ordering[A])
  inline def median: A = g.data.sorted(using n)(g.data.length/2)

object Matrix:
  extension [A : ClassTag](m: Matrix[1, 1, A])
    inline def singleElement: A = m(0, 0)
  inline def asSingleElement[A : ClassTag](a: A): Matrix[1, 1, A] = Matrix(Array(a))

  inline def from[M <: Int, N <: Int, A : ClassTag](as: IterableOnce[A]): Matrix[M, N, A] = Matrix(Array.from(as))

  inline def tabulate[M <: Int, N <: Int, A : ClassTag](inline f: (Int, Int) => A): Matrix[M, N, A] =
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
    Matrix(data)

  inline def fill[M <: Int, N <: Int, A : ClassTag](v: A): Matrix[M, N, A] = Matrix.tabulate((_, _) => v)
