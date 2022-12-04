/**
 * Main (Sized) Matrix implementation.
 * The Matrix dimensions have to be known at compiletime.
 * All functions are inlined, to access this information.
 *
 * Extension methods are available for specific type parameters.
 * These are useful for square matrices, Numeric matrices, etc.
 */
package be.adamv.macroloop.collection

import be.adamv.macroloop.collection.MatrixMNOps
import be.adamv.macroloop.{ArrayIndex, IntRange, IterableIt, SizedArrayIndex}

import scala.compiletime.constValue
import scala.compiletime.ops.int.*


// TODO likely want to abstract Matrix out to a trait, and allow different implementations.
// For example with an underlying vector VectorMatrix or specializing SquareMatrix
/**
 * Row-major Array Sized Matrix implementation with mutable entries.
 * @tparam M Rows
 * @tparam N Columns
 * @tparam A Element-type
 */
final class MatrixMNArray[M <: Int, N <: Int, A](final val data: Array[A]) extends AnyVal with MatrixMNOps[M, N, A, MatrixMNArray]:
  inline def factory: MatrixMNArray.type = MatrixMNArray

  /** Get the element at row i and column j. */
  inline def apply(inline i: Int, inline j: Int): A = data(i*ncolumns + j)
  /** Set the element at row i and column j to v. */
  inline def update(inline i: Int, inline j: Int, inline v: A): Unit = data(i*ncolumns + j) = v

  /** An iterator over all Matrix elements */
  inline def iterator: Iterator[A] = data.iterator
  /** An iterator over row-iterators. */
  inline def rowsIt: Iterator[Iterator[A]] = new collection.AbstractIterator[Iterator[A]]:
    var i = 0
    override def hasNext: Boolean = i < nrows
    override def next(): Iterator[A] =
      i += 1
      new collection.AbstractIterator[A]:
        private val begin = (i - 1)*ncolumns
        private val end = i*ncolumns
        private var j = begin
        override def hasNext: Boolean = j < end
        override def next(): A =
          val r = data(j)
          j += 1
          r
  /** An iterator over column-iterators. */
  inline def columnsIt: Iterator[Iterator[A]] = new collection.AbstractIterator[Iterator[A]]:
    var j = 0
    override def hasNext: Boolean = j < ncolumns
    override def next(): Iterator[A] =
      j += 1
      new collection.AbstractIterator[A]:
        private var i = j - 1
        override def hasNext: Boolean = i < nitems
        override def next(): A =
          val r = data(i)
          i += ncolumns
          r

//  /** Filter all elements based on their (row, column) index. */
//  inline def filterIndices(inline p: (Int, Int) => Boolean): Array[A] =
//    // TODO make a compiletime-informed builder
//    val cdata: Array[A] = SizedArrayIndex.ofSize[M*N, A]
//    var c = 0
//    var i = 0
//    while i < nrows do
//      var j = 0
//      while j < ncolumns do
//        if p(i, j) then
//          cdata(c) = this(i, j)
//          c += 1
//        j += 1
//      i += 1
//    Array.copyOf(cdata, c)

  /** Seq of all elements. */
  inline def toSeq: Seq[A] = collection.immutable.ArraySeq.unsafeWrapArray(data)
  /** Row-major Seq of all Seq rows. */
  inline def toSeqSeq: Seq[Seq[A]] = this.toSeq.grouped(ncolumns).toSeq

  /** Perform a side-effect to each element. */
  inline def forEach(inline f: A => Unit): Unit = ArrayIndex.forEach(data)(f)
  /** Same size matrix with its elements transformed by f. */
  inline def map[B](inline f: A => B): MatrixMNArray[M, N, B] =
    factory.wrap(SizedArrayIndex.mapForSize(data, constValue[M*N])(f))

  /** Checks if p holds for all items, exposing both the index and element. */
  inline def forallItem(inline p: (Int, Int, A) => Boolean): Boolean =
    var c = -1
    IntRange.forall(nrows) { i =>
      IntRange.forall(ncolumns) { j =>
        c += 1
        p(i, j, data(c))
      }
    }

  /** Checks if there exists an item for which p holds, exposing both the index and element. */
  inline def existsItem(inline p: (Int, Int, A) => Boolean): Boolean =
    var c = -1
    IntRange.exists(nrows){ i =>
      IntRange.exists(ncolumns) { j =>
        c += 1
        p(i, j, data(c))
      }
    }


  /** Generalized kronecker (outer) product for different input matrix types and output type. */
  inline def kronecker[O <: Int, P <: Int, B, C](that: MatrixMNArray[O, P, B],
                                                 inline combine: (A, B) => C): MatrixMNArray[M*O, N*P, C] =
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
    factory.wrap(cdata)

  /** Generalized hadamard (element-wise) product for different input matrix types and output type. */
  inline def hadamard[B, C](that: MatrixMNArray[M, N, B],
                            inline combine: (A, B) => C): MatrixMNArray[M, N, C] =
    val cdata: Array[C] = SizedArrayIndex.ofSize[M*N, C]
    IntRange.forEach(constValue[M*N])(i => cdata(i) = combine(this.data(i), that.data(i)))
    factory.wrap(cdata)

  /** Generalized frobenius (inner) product for different input matrix types and output type. */
  inline def frobenius[B, C](that: MatrixMNArray[M, N, B],
                             inline combine: (A, B) => C, inline add: (C, C) => C, inline zero: C): C =
    var c = zero
    var i = 0
    while i < nitems do
      c = add(c, combine(this.data(i), that.data(i)))
      i += 1
    c

  // FIXME standard equals is not-inlineable, so it doesn't allow checking dimensions
  /** Contains the same elements. */
  override def equals(that: Any): Boolean = that match
    case that: MatrixMNArray[m, n, _] => this.data sameElements that.data
    case _ => false


object MatrixMNArray extends MatrixMNFactory[Array, MatrixMNArray]:
  override type VectorType[N <: Int, A] = VectorNArray[N, A]
  override inline def vectorFactory: VectorNArray.type = VectorNArray

  transparent inline def apply[Tup <: NonEmptyTuple](inline elements: Tup): MatrixMNArray[Tuple.Size[Tup], Tuple.Size[AsTuple[Tuple.Head[Tup]]], Tuple.Union[Flatten[Tup]]] =
    ${ macros.concreteMatrixImpl[
      Tuple.Size[Tup],
      Tuple.Size[AsTuple[Tuple.Head[Tup]]],
      Tuple.Union[Flatten[Tup]]
    ]('elements) }


  /** Sizes and data array to Matrix. */
  inline def wrap[M <: Int, N <: Int, A](inline initial: Array[A]): MatrixMNArray[M, N, A] =
    assert(constValue[M*N] == initial.length) // NOTE not compiletime
    new MatrixMNArray(initial)

  /** Take M*N elements from an iterator to construct a Matrix. */
  inline def from[M <: Int, N <: Int, A](as: IterableOnce[A]): MatrixMNArray[M, N, A] =
    val data: Array[A] = SizedArrayIndex.ofSize[M*N, A]
    val written = as.iterator.copyToArray(data, 0, constValue[M*N])
    assert(written == constValue[M*N])
    new MatrixMNArray(data)

  /** Take M iterators from an iterator and take N of each of those to construct a Matrix. */
  inline def from2D[M <: Int, N <: Int, A](ass: IterableOnce[IterableOnce[A]]): MatrixMNArray[M, N, A] =
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
    new MatrixMNArray(data)

  /** Fill a matrix with elements dependent on their (row, column) position. */
  inline def tabulate[M <: Int, N <: Int, A](inline f: (Int, Int) => A): MatrixMNArray[M, N, A] =
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
    new MatrixMNArray(data)

  /** Fill a matrix with a certain element. */
  inline def fill[M <: Int, N <: Int, A](v: A): MatrixMNArray[M, N, A] =
    val data: Array[A] = SizedArrayIndex.ofSize[M*N, A]
    IntRange.forEach(constValue[M*N])(i => data(i) = v)
    new MatrixMNArray(data)

  extension [M <: Int, N <: Int, A](m: MatrixMNArray[M, N, A])
    inline def foldColumns[B](z: B)(op: (B, A) => B): VectorNArray[M, B] =
      val ar = SizedArrayIndex.ofSize[M, B]
      m.rowsIt.map(_.foldLeft(z)(op)).copyToArray(ar)
      vectorFactory.wrap(ar)
    inline def foldRows[B](z: B)(op: (B, A) => B): VectorNArray[N, B] =
      val ar = SizedArrayIndex.ofSize[N, B]
      m.columnsIt.map(_.foldLeft(z)(op)).copyToArray(ar)
      vectorFactory.wrap(ar)

//  extension [N <: Int, A](m: MatrixMNArray[N, N, A])
    // TODO unnecessary copy and allocation here; sized filterIndices or specialized implementation?
    /** Get the elements on the diagonal. */
//    inline def diagonal: SizedVector[N, A] = SizedVector.wrap(m.filterIndices(_ == _))

  // TODO the Fractional and Ordering abstractions are likely not in the interested of performance
  // FIXME since average and median don't utilize the matrix structure, they are bad examples
  extension [M <: Int, N <: Int, A] (g: MatrixMNArray[M, N, A])(using n: Fractional[A])
    /** Statistical average of all elements. */
    inline def average: A = n.div(g.data.fold(n.zero)(n.plus), n.fromInt(g.data.length))

  extension [M <: Int, N <: Int, A] (g: MatrixMNArray[M, N, A])(using n: Ordering[A])
    /** Statistical median of all elements. */
    inline def median: A = g.data.sorted(using n)(g.data.length/2)
