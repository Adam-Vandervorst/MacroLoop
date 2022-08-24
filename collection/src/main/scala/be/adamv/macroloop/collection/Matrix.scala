package be.adamv.macroloop.collection

import scala.compiletime.constValue
import scala.compiletime.ops.int.*
import be.adamv.macroloop.{ArrayIndex, IntRange, SizedArrayIndex}


class Matrix[M <: Int, N <: Int, A](val data: Array[A]):
  inline def nrows: M = constValue
  inline def ncolumns: N = constValue
  inline def nitems: M*N = constValue

  inline def apply(inline i: Int, inline j: Int): A = data(i*ncolumns + j)
  inline def update(inline i: Int, inline j: Int, inline v: A): Unit = data(i*ncolumns + j) = v

  inline def rows: Iterator[Array[A]] = data.grouped(ncolumns)

  inline def filterIndices(inline p: (Int, Int) => Boolean): Array[A] =
    // TODO, make a compiletime-informed builder
    val cdata: Array[A] = SizedArrayIndex.ofSize[M*N, A]
    var c = 0
    var j = 0
    while j < nrows do
      var i = 0
      while i < ncolumns do
        if p(j, i) then
          cdata(c) = this(j, i)
          c += 1
        i += 1
      j += 1
    Array.copyOf(cdata, c)

  inline def transpose: Matrix[N, M, A] = Matrix.tabulate((i, j) => this(j, i))
  inline def tiled[DM <: Int, DN <: Int](using M % DM =:= 0, N % DN =:= 0): Matrix[DM, DN, Matrix[M/DM, N/DN, A]] =
    Matrix.tabulate((di, dj) => Matrix.tabulate((i, j) => this(di*constValue[M/DM] + i, dj*constValue[N/DN] + j)))
  inline def slice[I1 <: Int, I2 <: Int, J1 <: Int, J2 <: Int]: Matrix[I2 - I1, J2 - J1, A] =
    Matrix.tabulate((i, j) => this(constValue[I1] + i, constValue[J1] + j))

  inline def toSeq: Seq[A] = collection.immutable.ArraySeq.unsafeWrapArray(data)
  inline def toSeqSeq: Seq[Seq[A]] = this.toSeq.grouped(ncolumns).toSeq

  inline def forEach(inline f: A => Unit): Unit = ArrayIndex.forEach(data)(f)
  inline def map[B](inline f: A => B): Matrix[M, N, B] =
    val ndata = SizedArrayIndex.ofSize[M*N, B]
    IntRange.forEach(0, constValue[M*N], 1)(i => ndata(i) = f(data(i)))
    Matrix(ndata)
  inline def flatMap[O <: Int, P <: Int, B](inline f: A => Matrix[O, P, B]): Matrix[M*O, N*P, B] = map(f).flatten

  inline def convolve[O <: Int, P <: Int, B, C](kernel: Matrix[O, P, B],
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

  inline def multiply[O <: Int, B, C](that: Matrix[N, O, B],
      inline combine: (A, B) => C, inline add: (C, C) => C, inline zero: C): Matrix[M, O, C] =
    val result = Matrix.fill[M, O, C](zero)

    for i <- 0 until this.nrows
        j <- 0 until that.ncolumns
        k <- 0 until this.ncolumns do
      result(i, j) = add(result(i, j), combine(this(i, k), that(k, j)))
    result

  inline def kronecker[O <: Int, P <: Int, B, C](that: Matrix[O, P, B],
                                                 inline combine: (A, B) => C): Matrix[M*O, N*P, C] =
    // workaround weird inlining "Term-dependent types are experimental" bug
    def inner(a: A) = that.map(combine(a, _))
    this.flatMap(inner)

  inline def hadamard[B, C](that: Matrix[M, N, B],
      inline combine: (A, B) => C): Matrix[M, N, C] =
    val cdata: Array[C] = SizedArrayIndex.ofSize[M*N, C]
    IntRange.forEach(0, constValue[M*N], 1)(i => cdata(i) = combine(this.data(i), that.data(i)))
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


object Matrix:
  inline def apply[M <: Int, N <: Int, A](data: Array[A]) = new Matrix[M, N, A](data)

  extension [A](m: Matrix[1, 1, A])
    inline def singleElement: A = m(0, 0)
  inline def asSingleElement[A](a: A): Matrix[1, 1, A] = Matrix.fill(a)

  inline def from[M <: Int, N <: Int, A](as: IterableOnce[A]): Matrix[M, N, A] =
    val data: Array[A] = SizedArrayIndex.ofSize[M*N, A]
    val it = as.iterator
    IntRange.forEach(0, constValue[M*N], 1)(i => data(i) = it.next())
    Matrix(data)

  inline def from2D[M <: Int, N <: Int, A](ass: IterableOnce[IterableOnce[A]]): Matrix[M, N, A] =
    val m = constValue[M]
    val n = constValue[N]
    val data: Array[A] = SizedArrayIndex.ofSize[M*N, A]
    val it = ass.iterator
    var j = 0
    while j < m do
      val it2 = it.next().iterator
      var i = 0
      while i < n do
        data(j*n + i) = it2.next()
        i += 1
      j += 1
    new Matrix(data)

  inline def fromSparse[M <: Int, N <: Int, A](pf: PartialFunction[(Int, Int), A]): Matrix[M, N, Option[A]] =
    Matrix.tabulate(pf.unapply(_, _))

  inline def tabulate[M <: Int, N <: Int, A](inline f: (Int, Int) => A): Matrix[M, N, A] =
    val m = constValue[M]
    val n = constValue[N]
    val data: Array[A] = SizedArrayIndex.ofSize[M*N, A]
    var j = 0
    while j < m do
      var i = 0
      while i < n do
        data(j*n + i) = f(j, i)
        i += 1
      j += 1
    new Matrix(data)

  inline def fill[M <: Int, N <: Int, A](v: A): Matrix[M, N, A] =
    val data: Array[A] = SizedArrayIndex.ofSize[M*N, A]
    IntRange.forEach(0, constValue[M*N], 1)(i => data(i) = v)
    Matrix(data)

  extension [M <: Int, N <: Int](m: Matrix[M, N, _])
    inline def isSquare: Boolean = m.nrows == m.ncolumns

  extension [N <: Int, A](m: Matrix[N, N, A])
    inline def diagonal: SizedVector[N, A] = SizedVector(m.filterIndices(_ == _))
    inline def isSymmetric: Boolean = m == m.transpose

  extension [M <: Int, N <: Int, O <: Int, P <: Int, A](nested: Matrix[M, N, Matrix[O, P, A]])
    inline def flatten: Matrix[M*O, N*P, A] =
      val o = constValue[O]; val p = constValue[P]
      Matrix.tabulate[M*O, N*P, A]((i, j) => nested(i/o, j/p)(i%o, j%p))

    inline def showNested: String = nested.rows
      .map(
        _.map(
          _.rows.map(
            _.map(
              _.toString
            ).mkString(" ")
          ).mkString("|")
        ).mkString("\n")
      ).mkString("\n\n")


  extension [M <: Int, N <: Int, A](g: Matrix[M, N, A])(using n: Fractional[A])
    inline def average: A = n.div(g.data.fold(n.zero)(n.plus), n.fromInt(g.data.length))

  extension [M <: Int, N <: Int, A](g: Matrix[M, N, A])(using n: Ordering[A])
    inline def median: A = g.data.sorted(using n)(g.data.length/2)
