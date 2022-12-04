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
import be.adamv.macroloop.{IntRange, IterableIt}


// TODO likely want to abstract Matrix out to a trait, and allow different implementations.
// For example with an underlying vector VectorMatrix or specializing SquareMatrix
/**
 * Row-major Array Sized Matrix implementation with mutable entries.
 * @tparam M Rows
 * @tparam N Columns
 * @tparam A Element-type
 */
trait MatrixMNOps[M <: Int, N <: Int, A, CC[M <: Int, N <: Int, A] <: MatrixMNOps[M, N, A, CC]] extends Any:
  inline def factory: MatrixMNFactory[_, CC]

  inline def nrows: M = constValue
  inline def ncolumns: N = constValue
  inline def nitems: M*N = constValue

  /** Get the element at row i and column j. */
  inline def apply(inline i: Int, inline j: Int): A
  /** Set the element at row i and column j to v. */
  inline def update(inline i: Int, inline j: Int, inline v: A): Unit
  /** Check if a position i, j is within the bounds of this matrix. */
  inline def containsPosition(inline i: Int, inline j: Int): Boolean =
    0 <= i && i < nrows && 0 <= j && j < ncolumns

  /** An iterator over all Matrix indices. */
  inline def indices: Iterator[(Int, Int)] = new collection.AbstractIterator[(Int, Int)]:
    var i = 0
    var j = 0
    override def hasNext: Boolean = i < nrows
    override def next(): (Int, Int) =
      val t = (i, j)
      if j < ncolumns - 1 then
        j += 1
      else
        i += 1
        j = 0
      t
  /** An iterator over all Matrix elements */
  inline def iterator: Iterator[A]
  /** An iterator over row-iterators. */
  inline def rowsIt: Iterator[Iterator[A]]
  /** An iterator over column-iterators. */
  inline def columnsIt: Iterator[Iterator[A]]

  // TODO have a separate (compiletime?) constant-time TransposedMatrix
  /** Eagerly transpose a matrix (swapping rows and columns). */
  inline def transpose: CC[N, M, A] = factory.tabulate((i, j) => this(j, i))
  /** Given row- and column-divisions DM and DN, divide up this matrix into smaller ones. */
  inline def tiled[DM <: Int, DN <: Int](using M % DM =:= 0, N % DN =:= 0): CC[DM, DN, CC[M/DM, N/DN, A]] =
    factory.tabulate((di, dj) => factory.tabulate((i, j) => this(di*constValue[M/DM] + i, dj*constValue[N/DN] + j)))
  /** Given start-row, end-row, start-column, and end-column, select the sub-matrix. */
  inline def slice[I1 <: Int, I2 <: Int, J1 <: Int, J2 <: Int]: CC[I2 - I1, J2 - J1, A] =
    factory.tabulate((i, j) => this(constValue[I1] + i, constValue[J1] + j))

  /** Seq of all elements. */
  inline def toSeq: Seq[A]
  /** Row-major Seq of all Seq rows. */
  inline def toSeqSeq: Seq[Seq[A]]

  /** Perform a side-effect to each element. */
  inline def forEach(inline f: A => Unit): Unit
  /** Same size matrix with its elements transformed by f. */
  inline def map[B](inline f: A => B): CC[M, N, B]
  /** Each element gets expanded into a sub-matrix by f. */
  inline def flatMap[O <: Int, P <: Int, B](inline f: A => CC[O, P, B]): CC[M*O, N*P, B] = factory.flatten(map(f))

  /** Checks if p holds for all items, exposing both the index and element. */
  inline def forallItem(inline p: (Int, Int, A) => Boolean): Boolean

  /** Checks if there exists an item for which p holds, exposing both the index and element. */
  inline def existsItem(inline p: (Int, Int, A) => Boolean): Boolean

  // TODO what happens if the kernel is larger than the matrix here?
  /** Move kernel over the matrix, combining all overlapping pairs, and accumulating them with add and zero into a new entry. */
  inline def convolve[O <: Int, P <: Int, B, C](kernel: CC[O, P, B],
                                                inline combine: (A, B) => C, inline add: (C, C) => C, inline zero: C): CC[M, N, C] =
    val cx = kernel.nrows/2
    val cy = kernel.ncolumns/2

    val result = factory.fill[M, N, C](zero)

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
  inline def multiply[O <: Int, B, C](that: CC[N, O, B],
                                      inline combine: (A, B) => C, inline add: (C, C) => C, inline zero: C): CC[M, O, C] =
    val result = factory.fill[M, O, C](zero)

    for i <- 0 until this.nrows
        j <- 0 until that.ncolumns
        k <- 0 until this.ncolumns do
      result(i, j) = add(result(i, j), combine(this(i, k), that(k, j)))
    result

  /** Generalized kronecker (outer) product for different input matrix types and output type. */
  inline def kronecker[O <: Int, P <: Int, B, C](that: CC[O, P, B],
                                                 inline combine: (A, B) => C): CC[M*O, N*P, C]

  /** Generalized hadamard (element-wise) product for different input matrix types and output type. */
  inline def hadamard[B, C](that: CC[M, N, B],
                            inline combine: (A, B) => C): CC[M, N, C]

  /** Generalized frobenius (inner) product for different input matrix types and output type. */
  inline def frobenius[B, C](that: CC[M, N, B],
                             inline combine: (A, B) => C, inline add: (C, C) => C, inline zero: C): C

  // FIXME standard toString is not-inlineable, so it doesn't allow a dimension-based representation
  /** Pretty string, do not rely on the precise output. */
  inline def show: String =
    // TODO show equal-width columns, compress representation for large matrices
    rowsIt.map(row => row.mkString(",")).mkString("\n")

  // FIXME standard equals is not-inlineable, so it doesn't allow checking dimensions
  /** Contains the same elements. */
  override def equals(that: Any): Boolean


trait MatrixMNFactory[Inner[_], CC[M <: Int, N <: Int, A] <: MatrixMNOps[M, N, A, CC]]:
  type VectorType[N <: Int, A] <: VectorNOps[N, A, VectorType]
  inline def vectorFactory: VectorNFactory[Inner, VectorType]

  type AsTuple[X] <: Tuple = X match
    case Tuple => X & Tuple
  type Flatten[Tup <: Tuple] =
    Tuple.FlatMap[Tup, [X <: Tuple.Union[Tup]] =>> AsTuple[X]]

  transparent inline def apply[Tup <: NonEmptyTuple](inline elements: Tup): CC[Tuple.Size[Tup], Tuple.Size[AsTuple[Tuple.Head[Tup]]], Tuple.Union[Flatten[Tup]]]

  /** Sizes and data array to Matrix. */
  inline def wrap[M <: Int, N <: Int, A](inline initial: Inner[A]): CC[M, N, A]

  // TODO decide on what to do with 0-element matrices
  extension [A](m: CC[1, 1, A])
    inline def singleElement: A = m(0, 0)
  inline def asSingleElement[A](a: A): CC[1, 1, A] = apply(Tuple1(Tuple1(a)))

  /** Take M*N elements from an iterator to construct a Matrix. */
  inline def from[M <: Int, N <: Int, A](as: IterableOnce[A]): CC[M, N, A]

  /** Take M iterators from an iterator and take N of each of those to construct a Matrix. */
  inline def from2D[M <: Int, N <: Int, A](ass: IterableOnce[IterableOnce[A]]): CC[M, N, A]

  // TODO this probably not the expected behavior
  // TODO have a sparse class too?
  /** Like tabulate but for partial functions. */
  inline def fromSparse[M <: Int, N <: Int, A](pf: PartialFunction[(Int, Int), A]): CC[M, N, Option[A]] =
    tabulate(pf.unapply(_, _))

  /** Fill a matrix with elements dependent on their (row, column) position. */
  inline def tabulate[M <: Int, N <: Int, A](inline f: (Int, Int) => A): CC[M, N, A]

  /** Fill a matrix with a certain element. */
  inline def fill[M <: Int, N <: Int, A](v: A): CC[M, N, A]

  extension [M <: Int, N <: Int, A](m: CC[M, N, A])
    inline def foldColumns[B](z: B)(op: (B, A) => B): VectorType[M, B]
    inline def foldRows[B](z: B)(op: (B, A) => B): VectorType[N, B]

  // TODO implement as constant type-based functions?
  extension [M <: Int, N <: Int, A](m: CC[M, N, A])
    /** Check if a matrix is square. */
    inline def isSquare: Boolean = m.nrows == m.ncolumns
    /** Check if a matrix has cycles. */
    inline def hasCycle: Boolean =
      val visited = fill[M, N, Boolean](false)

      val directionX: Array[Int] = Array(-1, 0, 1, 0)
      val directionY: Array[Int] = Array(0, 1, 0, -1)

      def isCycle(x: Int, y: Int, px: Int = -1, py: Int = -1): Boolean =
        visited(x, y) = true

        IntRange.existsUnrolled(0, 4, 1) { k =>
          val nx = x + directionX(k)
          val ny = y + directionY(k)
          m.containsPosition(nx, ny) &&
          m(nx, ny) == m(x, y) &&
          !(px == nx && py == ny) && (
            visited(nx, ny) ||
            isCycle(nx, ny, x, y)
          )
        }
      end isCycle

      visited.existsItem((i, j, visitedij) => !visitedij && isCycle(i, j))
    end hasCycle

  extension [N <: Int, A](m: CC[N, N, A])
    // TODO unnecessary copy and allocation here; sized filterIndices or specialized implementation?
    /** Get the elements on the diagonal. */
    inline def diagonal: VectorType[N, A] =
      vectorFactory.tabulate(i => m(i, i))
    // TODO this has a more efficient implementation
    /** Check if a matrix is mirrored over its diagonal. */
    inline def isSymmetric: Boolean = m == m.transpose

  // TODO add asVectors conversion using copyRange

  extension [M <: Int, N <: Int, O <: Int, P <: Int, A](nested: CC[M, N, CC[O, P, A]])
    /** Flatten out a matrix containing matrices with the appropriate sizes. */
    inline def flatten: CC[M*O, N*P, A] =
      val o = constValue[O]; val p = constValue[P]
      tabulate[M*O, N*P, A]((i, j) => nested(i/o, j/p)(i%o, j%p))

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
  extension [M <: Int, N <: Int, A] (g: CC[M, N, A])(using n: Fractional[A])
    /** Statistical average of all elements. */
    inline def average: A

  extension [M <: Int, N <: Int, A] (g: CC[M, N, A])(using n: Ordering[A])
    /** Statistical median of all elements. */
    inline def median: A
