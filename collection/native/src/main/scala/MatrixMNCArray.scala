package be.adamv.macroloop.collection


import be.adamv.macroloop.{IntRange, IterableIt}

import scalanative.unsafe.Ptr

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
abstract class MatrixMNCArray[M <: Int, N <: Int, A] extends MatrixMNOps[M, N, A, MatrixMNCArray]:
  override inline def factory: MatrixMNCArray.type = MatrixMNCArray
  val ptr: Ptr[A]

  /** Get the element at row i and column j. */
  inline def apply(inline i: Int, inline j: Int): A = ???
  /** Set the element at row i and column j to v. */
  inline def update(inline i: Int, inline j: Int, inline v: A): Unit = ???

  /** An iterator over all Matrix elements */
  inline def iterator: Iterator[A] = ???
  /** An iterator over row-iterators. */
  inline def rowsIt: Iterator[Iterator[A]] = ???
  /** An iterator over column-iterators. */
  inline def columnsIt: Iterator[Iterator[A]] = ???

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
  inline def toSeq: Seq[A] = ???
  /** Row-major Seq of all Seq rows. */
  inline def toSeqSeq: Seq[Seq[A]] = ???

  /** Perform a side-effect to each element. */
  inline def forEach(inline f: A => Unit): Unit = ???
  /** Same size matrix with its elements transformed by f. */
  inline def map[B](inline f: A => B): MatrixMNCArray[M, N, B] = ???

  /** Checks if p holds for all items, exposing both the index and element. */
  inline def forallItem(inline p: (Int, Int, A) => Boolean): Boolean = ???

  /** Checks if there exists an item for which p holds, exposing both the index and element. */
  inline def existsItem(inline p: (Int, Int, A) => Boolean): Boolean = ???

  /** Generalized kronecker (outer) product for different input matrix types and output type. */
  inline def kronecker[O <: Int, P <: Int, B, C](that: MatrixMNCArray[O, P, B],
                                                 inline combine: (A, B) => C): MatrixMNCArray[M*O, N*P, C] = ???

  /** Generalized hadamard (element-wise) product for different input matrix types and output type. */
  inline def hadamard[B, C](that: MatrixMNCArray[M, N, B],
                            inline combine: (A, B) => C): MatrixMNCArray[M, N, C] = ???

  /** Generalized frobenius (inner) product for different input matrix types and output type. */
  inline def frobenius[B, C](that: MatrixMNCArray[M, N, B],
                             inline combine: (A, B) => C, inline add: (C, C) => C, inline zero: C): C = ???

  // FIXME standard equals is not-inlineable, so it doesn't allow checking dimensions
  /** Contains the same elements. */
  override def equals(that: Any): Boolean = that match
    case that: MatrixMNCArray[m, n, _] => ???
    case _ => false


object MatrixMNCArray extends MatrixMNFactory[Ptr, MatrixMNCArray]:
  override type VectorType[N <: Int, A] = VectorNCArray[N, A]
  override inline def vectorFactory: VectorNCArray.type = VectorNCArray

  transparent inline def apply[Tup <: NonEmptyTuple](inline elements: Tup): MatrixMNCArray[Tuple.Size[Tup], Tuple.Size[AsTuple[Tuple.Head[Tup]]], Tuple.Union[Flatten[Tup]]] =
    ???
//    ${ macros.concreteMatrixImpl[
//      Tuple.Size[Tup],
//      Tuple.Size[AsTuple[Tuple.Head[Tup]]],
//      Tuple.Union[Flatten[Tup]]
//    ]('elements) }


  /** Sizes and data array to Matrix. */
  inline def wrap[M <: Int, N <: Int, A](inline initial: Ptr[A]): MatrixMNCArray[M, N, A] =
    new:
      override val ptr: Ptr[A] = ???

  /** Take M*N elements from an iterator to construct a Matrix. */
  inline def from[M <: Int, N <: Int, A](as: IterableOnce[A]): MatrixMNCArray[M, N, A] = ???

  /** Take M iterators from an iterator and take N of each of those to construct a Matrix. */
  inline def from2D[M <: Int, N <: Int, A](ass: IterableOnce[IterableOnce[A]]): MatrixMNCArray[M, N, A] = ???

  /** Fill a matrix with elements dependent on their (row, column) position. */
  inline def tabulate[M <: Int, N <: Int, A](inline f: (Int, Int) => A): MatrixMNCArray[M, N, A] = ???

  /** Fill a matrix with a certain element. */
  inline def fill[M <: Int, N <: Int, A](v: A): MatrixMNCArray[M, N, A] = ???

  extension [M <: Int, N <: Int, A](m: MatrixMNCArray[M, N, A])
    inline def foldColumns[B](z: B)(op: (B, A) => B): VectorNCArray[M, B] = ???
    inline def foldRows[B](z: B)(op: (B, A) => B): VectorNCArray[N, B] = ???

//  extension [N <: Int, A](m: MatrixMNCArray[N, N, A])
    // TODO unnecessary copy and allocation here; sized filterIndices or specialized implementation?
    /** Get the elements on the diagonal. */
//    inline def diagonal: SizedVector[N, A] = SizedVector.wrap(m.filterIndices(_ == _))

  // TODO the Fractional and Ordering abstractions are likely not in the interested of performance
  // FIXME since average and median don't utilize the matrix structure, they are bad examples
  extension [M <: Int, N <: Int, A] (g: MatrixMNCArray[M, N, A])(using n: Fractional[A])
    /** Statistical average of all elements. */
    inline def average: A = ???

  extension [M <: Int, N <: Int, A] (g: MatrixMNCArray[M, N, A])(using n: Ordering[A])
    /** Statistical median of all elements. */
    inline def median: A = ???
