package be.adamv.macroloop.collection

import be.adamv.macroloop.collection.MatrixMNCArray

import be.adamv.macroloop.{IntRange}

import scalanative.unsafe.{CArray, Ptr, stackalloc}
import scalanative.libc.string.memcpy

import scala.compiletime.constValue
import scala.compiletime.ops.int.*


/**
 * Array-backed Sized Vector imlementation with mutable entries.
 * @tparam N Elements
 * @tparam A Element-type
 */
abstract class VectorNCArray[N <: Int, A] extends VectorNOps[N, A, VectorNCArray]:
  override inline def factory: VectorNCArray.type = VectorNCArray
  val ptr: Ptr[A]

  /** Get the element in position i. */
  inline def apply(inline i: Int): A = ??? //ptr.apply(i)
  /** Set the element in position i to v. */
  inline def update(inline i: Int, inline v: A): Unit = ??? //ptr(i) = v

  /** An iterator over all elements. */
  inline def iterator: Iterator[A] =
    ???

  inline def forEach(inline f: A => Unit): Unit = ???
  inline def kronecker[M <: Int, B, C]
  (that: be.adamv.macroloop.collection.VectorNCArray[M, B], inline combine: (A, B) => C
    )
  : be.adamv.macroloop.collection.VectorNCArray[M * N, C] = ???
  inline def map[B](inline f: A => B): be.adamv.macroloop.collection.VectorNCArray[N, B] = ???
  inline def show: String = ???
  inline def slice[I1 <: Int, I2 <: Int]:
  be.adamv.macroloop.collection.VectorNCArray[I2 - I1, A] = ???
  inline def toSeq: Seq[A] = ???

  /** Divide up this vector in DN chunks, creating a vector of vectors. */
  inline def chunked[DN <: Int](using N % DN =:= 0): VectorNCArray[DN, VectorNCArray[N/DN, A]] = ???
//    val isize = constValue[N/DN]
//    factory.tabulate { i =>
//      val iptr = alloc[CArray[A, IntToNat[N/DN]]]
//      memcpy(iptr, ptr + i, isize)
//      factory.wrap(iptr)
//    }
  /** Take an I1 to I2 slice. */
//  inline def slice[I1 <: Int, I2 <: Int]: VectorNCArray[I2 - I1, A] = ???

  /** Seq of all elements. */
//  inline def toSeq: Seq[A] = ???

  /** Perform a side-effect to each element. */
//  inline def forEach(inline f: A => Unit): Unit = ???
  /** Same size vector with its elements transformed by f. */
//  inline def map[B](inline f: A => B): VectorNCArray[N, B] = ???
  /** Each element gets expanded into a sub-vector by f. */
  inline def flatMap[M <: Int, B](inline f: A => VectorNCArray[M, B]): VectorNCArray[M*N, B] = ???

  // FIXME standard toString is not-inlineable, so it doesn't allow a dimension-based representation
  /** Pretty string, do not rely on the precise output. */
//  inline def show: String = ???

  /** Equality on vectors. */
  override def equals(that: Any): Boolean = that match
    case that: VectorNCArray[n, _] => ???
    case _ => false


object VectorNCArray extends VectorNFactory[Ptr, VectorNCArray]:
  type MatrixType[M <: Int, N <: Int, A] = MatrixMNCArray[M, N, A]
  inline def matrixFactory: MatrixMNCArray.type = MatrixMNCArray

  transparent inline def apply[Tup <: Tuple](inline elements: Tup): VectorNCArray[Tuple.Size[Tup], Tuple.Union[Tup]] =
    ???
//    ${ be.adamv.macroloop.macros.concreteVectorImpl[Tuple.Size[Tup], Tuple.Union[Tup]]('elements) }

  /** Size and data array to SizedVector. */
  inline def wrap[N <: Int, A](inline initial: Ptr[A]): VectorNCArray[N, A] =
    new:
      override val ptr: Ptr[A] = initial

  /** Take N elements from an iterator to construct a SizedVector. */
  inline def from[N <: Int, A](as: IterableOnce[A]): VectorNCArray[N, A] = ???

  /** Fill a SizedVector with elements dependent on their integer position. */
  inline def tabulate[N <: Int, A](inline f: Int => A): VectorNCArray[N, A] = ???

  extension [N <: Int, A](v: VectorNCArray[N, A])
    /** Interpret the flat vector in a Matrix with given dimension. */
    inline def reshape[O <: Int, P <: Int](using O*P =:= N): MatrixMNCArray[O, P, A] = ???
    /** Interpret the vector as a single-row matrix. */
    inline def asRow: MatrixMNCArray[1, N, A] = ???
    /** Interpret the vector as a single-column matrix. */
    inline def asColumn: MatrixMNCArray[N, 1, A] = ???
