/**
 * Main (Sized) Matrix implementation.
 * The Matrix dimensions have to be known at compiletime.
 * All functions are inlined, to access this information.
 *
 * Extension methods are available for specific type parameters.
 * These are useful for square matrices, Numeric matrices, etc.
 */
package be.adamv.macroloop.collection

import scala.compiletime.constValue
import scala.compiletime.ops.int.*
import be.adamv.macroloop.{ArrayIndex, IntRange, SizedArrayIndex, IterableIt}


// TODO likely want to abstract Matrix out to a trait, and allow different implementations.
// For example with an underlying vector VectorMatrix or specializing SquareMatrix
/**
 * Row-major Array Sized Matrix implementation with mutable entries.
 * @tparam M Rows
 * @tparam N Columns
 * @tparam A Element-type
 */
abstract class Matrix[M <: Int, N <: Int, A]:
  val data: Array[A]
  inline def nrows: M = constValue
  inline def ncolumns: N = constValue
  inline def nitems: M*N = constValue

  /** Get the element at row i and column j. */
  inline def apply(inline i: Int, inline j: Int): A = data(i*ncolumns + j)
  /** Set the element at row i and column j to v. */
  inline def update(inline i: Int, inline j: Int, inline v: A): Unit = data(i*ncolumns + j) = v

  /** An iterator over all Matrix elements */
  inline def iterator: Iterator[A] = data.iterator
  /** An iterator over row-iterators. */
  inline def rowsIt: Iterator[Iterator[A]] = new Iterator[Iterator[A]]:
    var i = 0
    override def hasNext: Boolean = i < nrows
    override def next(): Iterator[A] =
      i += 1
      new Iterator[A]:
        private val begin = (i - 1)*ncolumns
        private val end = i*ncolumns
        private var j = begin
        override def hasNext: Boolean = j < end
        override def next(): A =
          val r = data(j)
          j += 1
          r

  /** An iterator over column-iterators. */
  inline def columnsIt: Iterator[Iterator[A]] = new Iterator[Iterator[A]]:
    var j = 0
    override def hasNext: Boolean = j < ncolumns
    override def next(): Iterator[A] =
      j += 1
      new Iterator[A]:
        private var i = j - 1
        override def hasNext: Boolean = i < nitems
        override def next(): A =
          val r = data(i)
          i += ncolumns
          r

  /** Filter all elements based on their (row, column) index. */
  inline def filterIndices(inline p: (Int, Int) => Boolean): Array[A] =
    // TODO make a compiletime-informed builder
    val cdata: Array[A] = SizedArrayIndex.ofSize[M*N, A]
    var c = 0
    var i = 0
    while i < nrows do
      var j = 0
      while j < ncolumns do
        if p(i, j) then
          cdata(c) = this(i, j)
          c += 1
        j += 1
      i += 1
    Array.copyOf(cdata, c)

  // TODO have a separate (compiletime?) constant-time TransposedMatrix
  /** Eagerly transpose a matrix (swapping rows and columns). */
  inline def transpose: Matrix[N, M, A] = Matrix.tabulate((i, j) => this(j, i))
  /** Given row- and column-divisions DM and DN, divide up this matrix into smaller ones. */
  inline def tiled[DM <: Int, DN <: Int](using M % DM =:= 0, N % DN =:= 0): Matrix[DM, DN, Matrix[M/DM, N/DN, A]] =
    Matrix.tabulate((di, dj) => Matrix.tabulate((i, j) => this(di*constValue[M/DM] + i, dj*constValue[N/DN] + j)))
  /** Given start-row, end-row, start-column, and end-column, select the sub-matrix. */
  inline def slice[I1 <: Int, I2 <: Int, J1 <: Int, J2 <: Int]: Matrix[I2 - I1, J2 - J1, A] =
    Matrix.tabulate((i, j) => this(constValue[I1] + i, constValue[J1] + j))

  /** Seq of all elements. */
  inline def toSeq: Seq[A] = collection.immutable.ArraySeq.unsafeWrapArray(data)
  /** Row-major Seq of all Seq rows. */
  inline def toSeqSeq: Seq[Seq[A]] = this.toSeq.grouped(ncolumns).toSeq

  /** Perform a side-effect to each element. */
  inline def forEach(inline f: A => Unit): Unit = ArrayIndex.forEach(data)(f)
  /** Same size matrix with its elements transformed by f. */
  inline def map[B](inline f: A => B): Matrix[M, N, B] =
    val ndata = SizedArrayIndex.ofSize[M*N, B]
    IntRange.forEach(0, constValue[M*N], 1)(i => ndata(i) = f(data(i)))
    Matrix.wrap(ndata)
  /** Each element gets expanded into a sub-matrix by f. */
  inline def flatMap[O <: Int, P <: Int, B](inline f: A => Matrix[O, P, B]): Matrix[M*O, N*P, B] = map(f).flatten

  // TODO what happens if the kernel is larger than the matrix here?
  /** Move kernel over the matrix, combining all overlapping pairs, and accumulating them with add and zero into a new entry. */
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

  /** Generalized matrix multiplication for different input matrix types and output type. */
  inline def multiply[O <: Int, B, C](that: Matrix[N, O, B],
      inline combine: (A, B) => C, inline add: (C, C) => C, inline zero: C): Matrix[M, O, C] =
    val result = Matrix.fill[M, O, C](zero)

    for i <- 0 until this.nrows
        j <- 0 until that.ncolumns
        k <- 0 until this.ncolumns do
      result(i, j) = add(result(i, j), combine(this(i, k), that(k, j)))
    result

  /** Generalized kronecker (outer) product for different input matrix types and output type. */
  inline def kronecker[O <: Int, P <: Int, B, C](that: Matrix[O, P, B],
                                                 inline combine: (A, B) => C): Matrix[M*O, N*P, C] =
    val N = ncolumns
    val O = that.nrows
    val P = that.ncolumns
    val H = N*P
    val cdata = SizedArrayIndex.ofSize[M*O*N*P, C]
    var s = 0
    while s < nitems do
      val sr = (s / N)*O
      val sc = (s % N)*P
      val a = data(s)
      var t = 0
      while t < that.nitems do
        val tr = t / P
        val tc = t % P
        val b = that.data(t)
        val c = combine(a, b)
        cdata((sr + tr)*H + (sc + tc)) = c
        t += 1
      s += 1
    Matrix.wrap(cdata)

  /** Generalized hadamard (element-wise) product for different input matrix types and output type. */
  inline def hadamard[B, C](that: Matrix[M, N, B],
      inline combine: (A, B) => C): Matrix[M, N, C] =
    val cdata: Array[C] = SizedArrayIndex.ofSize[M*N, C]
    IntRange.forEach(0, constValue[M*N], 1)(i => cdata(i) = combine(this.data(i), that.data(i)))
    Matrix.wrap(cdata)

  /** Generalized frobenius (inner) product for different input matrix types and output type. */
  inline def frobenius[B, C](that: Matrix[M, N, B],
      inline combine: (A, B) => C, inline add: (C, C) => C, inline zero: C): C =
    var c = zero
    var i = 0
    while i < nitems do
      c = add(c, combine(this.data(i), that.data(i)))
      i += 1
    c

  // FIXME standard toString is not-inlineable, so it doesn't allow a dimension-based representation
  /** Pretty string, do not rely on the precise output. */
  inline def show: String =
    // TODO show equal-width columns, compress representation for large matrices
    rowsIt.map(row => row.mkString(",")).mkString("\n")

  // FIXME standard equals is not-inlineable, so it doesn't allow checking dimensions
  /** Contains the same elements. */
  override def equals(that: Any): Boolean = that match
    case that: Matrix[m, n, _] => this.data sameElements that.data
    case _ => false


object Matrix:
  export be.adamv.macroloop.collection.TupleConstructors.matrixApply as apply

  /** Sizes and data array to Matrix. */
  inline def wrap[M <: Int, N <: Int, A](inline initial: Array[A]): Matrix[M, N, A] =
    assert(constValue[M*N] == initial.length) // NOTE not compiletime
    new:
      override val data: Array[A] = initial

  // TODO decide on what to do with 0-element matrices
  extension [A](m: Matrix[1, 1, A])
    inline def singleElement: A = m(0, 0)
  inline def asSingleElement[A](a: A): Matrix[1, 1, A] = Matrix.fill(a)

  /** Take M*N elements from an iterator to construct a Matrix. */
  inline def from[M <: Int, N <: Int, A](as: IterableOnce[A]): Matrix[M, N, A] =
    val data: Array[A] = SizedArrayIndex.ofSize[M*N, A]
    val written = as.iterator.copyToArray(data, 0, constValue[M*N])
    assert(written == constValue[M*N])
    Matrix.wrap(data)

  /** Take M iterators from an iterator and take N of each of those to construct a Matrix. */
  inline def from2D[M <: Int, N <: Int, A](ass: IterableOnce[IterableOnce[A]]): Matrix[M, N, A] =
    val m = constValue[M]
    val n = constValue[N]
    val size = m*n
    val data: Array[A] = SizedArrayIndex.ofSize[M*N, A]
    val it = ass.iterator
    var k = 0
    while k < size && it.hasNext do
      val innerit = it.next().iterator
      val nk = k + m
      val written = innerit.copyToArray(data, k, nk)
      assert(written == m)
      k = nk
    Matrix.wrap(data)

  // TODO this probably not the expected behavior
  // TODO have a sparse class too?
  /** Like tabulate but for partial functions. */
  inline def fromSparse[M <: Int, N <: Int, A](pf: PartialFunction[(Int, Int), A]): Matrix[M, N, Option[A]] =
    Matrix.tabulate(pf.unapply(_, _))

  /** Fill a matrix with elements dependent on their (row, column) position. */
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
    Matrix.wrap(data)

  /** Fill a matrix with a certain element. */
  inline def fill[M <: Int, N <: Int, A](v: A): Matrix[M, N, A] =
    val data: Array[A] = SizedArrayIndex.ofSize[M*N, A]
    IntRange.forEach(0, constValue[M*N], 1)(i => data(i) = v)
    Matrix.wrap(data)

  extension [M <: Int, N <: Int, A](m: Matrix[M, N, A])
    inline def foldRows[B](z: B)(op: (B, A) => B): SizedVector[M, B] =
      val ar = SizedArrayIndex.ofSize[M, B]
      m.rowsIt.map(_.foldLeft(z)(op)).copyToArray(ar)
      SizedVector.wrap(ar)
    inline def foldColumns[B](z: B)(op: (B, A) => B): SizedVector[N, B] =
      val ar = SizedArrayIndex.ofSize[N, B]
      m.columnsIt.map(_.foldLeft(z)(op)).copyToArray(ar)
      SizedVector.wrap(ar)

  // TODO implement as constant type-based functions?
  extension [M <: Int, N <: Int](m: Matrix[M, N, _])
    /** Check if a matrix is square. */
    inline def isSquare: Boolean = m.nrows == m.ncolumns

  extension [N <: Int, A](m: Matrix[N, N, A])
    // TODO unnecessary copy and allocation here; sized filterIndices or specialized implementation?
    /** Get the elements on the diagonal. */
    inline def diagonal: SizedVector[N, A] = SizedVector.wrap(m.filterIndices(_ == _))
    // TODO this has a more efficient implementation
    /** Check if a matrix is mirrored over its diagonal. */
    inline def isSymmetric: Boolean = m == m.transpose

  // TODO add asVectors conversion using copyRange

  extension [M <: Int, N <: Int, O <: Int, P <: Int, A](nested: Matrix[M, N, Matrix[O, P, A]])
    /** Flatten out a matrix containing matrices with the appropriate sizes. */
    inline def flatten: Matrix[M*O, N*P, A] =
      val o = constValue[O]; val p = constValue[P]
      Matrix.tabulate[M*O, N*P, A]((i, j) => nested(i/o, j/p)(i%o, j%p))

    // TODO show equal-width columns per vertical stack, compress representation for large inner and outer matrices
    /** Specialized representation for matrix containing matrices. */
    inline def showNested: String = nested.rowsIt
      .map(
        _.map(
          _.rowsIt.map(
            _.mkString(" ")
          ).mkString("|")
        ).mkString("\n")
      ).mkString("\n\n")


  // TODO the Fractional and Ordering abstractions are likely not in the interested of performance
  // FIXME since average and median don't utilize the matrix structure, they are bad examples
  extension [M <: Int, N <: Int, A](g: Matrix[M, N, A])(using n: Fractional[A])
    /** Statistical average of all elements. */
    inline def average: A = n.div(g.data.fold(n.zero)(n.plus), n.fromInt(g.data.length))

  extension [M <: Int, N <: Int, A](g: Matrix[M, N, A])(using n: Ordering[A])
    /** Statistical median of all elements. */
    inline def median: A = g.data.sorted(using n)(g.data.length/2)
