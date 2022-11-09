/**
 * Main Sized Vector implementation
 * The Vector dimensions have to be known at compiletime.
 * All functions are inlined, to access this information.
 *
 * Extension methods are available for specific type parameters.
 * These are useful for even-dimensional Vectors, Numeric Vectors, etc.
 */
package be.adamv.macroloop.collection

import be.adamv.macroloop.IntRange

import scala.compiletime.constValue
import scala.compiletime.ops.int.*

/**
 * Array-backed Sized Vector imlementation with mutable entries.
 * @tparam N Elements
 * @tparam A Element-type
 */
abstract class VectorNOps[N <: Int, A, CC[N <: Int, A] <: VectorNOps[N, A, CC]]:
  inline def factory: VectorNFactory[_, CC]

  inline def length: N = constValue

  /** Get the element in position i. */
  inline def apply(inline i: Int): A
  /** Set the element in position i to v. */
  inline def update(inline i: Int, inline v: A): Unit

  /** An iterator over all elements. */
  inline def iterator: Iterator[A]

  /** Divide up this vector in DN chunks, creating a vector of vectors. */
  inline def chunked[DN <: Int](using N % DN =:= 0): CC[DN, CC[N/DN, A]]
  /** Take an I1 to I2 slice. */
  inline def slice[I1 <: Int, I2 <: Int]: CC[I2 - I1, A]

  /** Seq of all elements. */
  inline def toSeq: Seq[A]

  /** Perform a side-effect to each element. */
  inline def forEach(inline f: A => Unit): Unit
  /** Same size vector with its elements transformed by f. */
  inline def map[B](inline f: A => B): CC[N, B]
  /** Each element gets expanded into a sub-vector by f. */
  inline def flatMap[M <: Int, B](inline f: A => CC[M, B]): CC[M*N, B]

  /** Move kernel over the vector, combining all overlapping pairs, and accumulating them with add and zero into a new entry. */
  inline def convolve[M <: Int, B, C](kernel: CC[M, B],
                                      inline combine: (A, B) => C, inline add: (C, C) => C, inline zero: C): CC[N, C] =
    val c = kernel.length/2

    val res = factory.fill[N, C](zero)

    for i <- 0 until length
        k <- 0 until kernel.length do
      val ii = i + (k - c)

      if ii >= 0 && ii < length then
        res(i) = add(res(i), combine(this(ii), kernel(k)))
    res

  /** Generalized kronecker (outer) product for different input vector types and output type, flattened. */
  inline def kronecker[M <: Int, B, C](that: CC[M, B],
                                       inline combine: (A, B) => C): CC[M*N, C]

  /** Generalized element-wise product for different input vector types and output type. */
  inline def elementwise[B, C](that: CC[N, B],
                               inline combine: (A, B) => C): CC[N, C] =
    factory.tabulate { i =>
      combine(this(i), that(i))
    }

  /** Generalized inner product for different input vector types and output type. */
  inline def inner[B, C](that: CC[N, B],
                         inline combine: (A, B) => C, inline add: (C, C) => C, inline zero: C): C =
    var result = zero
    IntRange.forEach(length)(i => result = add(result, combine(this(i), that(i))))
    result

  // FIXME standard toString is not-inlineable, so it doesn't allow a dimension-based representation
  /** Pretty string, do not rely on the precise output. */
  inline def show: String

  /** Equality on vectors. */
  override def equals(that: Any): Boolean


trait VectorNFactory[Inner[_], CC[N <: Int, A] <: VectorNOps[N, A, CC]]:
  type MatrixType[M <: Int, N <: Int, A] <: MatrixMNOps[M, N, A, MatrixType]
  inline def matrixFactory: MatrixMNFactory[Inner, MatrixType]

  transparent inline def apply[Tup <: Tuple](inline elements: Tup): CC[Tuple.Size[Tup], Tuple.Union[Tup]]

  /** Size and data array to SizedVector. */
  inline def wrap[N <: Int, A](inline initial: Inner[A]): CC[N, A]

  extension [A](v: CC[1, A])
    inline def singleElement: A = v(0)
  inline def asSingleElement[A](a: A): CC[1, A] = apply(Tuple1(a))

  /** Take N elements from an iterator to construct a SizedVector. */
  inline def from[N <: Int, A](as: IterableOnce[A]): CC[N, A]

  // TODO this probably not the expected behavior
  // TODO have a sparse class too?
  /** Like tabulate but for partial functions. */
  inline def fromSparse[N <: Int, A](pf: PartialFunction[Int, A]): CC[N, Option[A]] =
    tabulate(pf.unapply)

  /** Fill a SizedVector with elements dependent on their integer position. */
  inline def tabulate[N <: Int, A](inline f: Int => A): CC[N, A]

  /** Fill a vector with a certain element. */
  inline def fill[N <: Int, A](v: A): CC[N, A] = tabulate(_ => v)

  extension [M <: Int, N <: Int, A](nested: CC[M, CC[N, A]])
    // TODO use copy primitives
    /** Interpreter the inner vectors as rows of a matrix. */
    inline def toMatrix: MatrixType[M, N, A] =
      matrixFactory.tabulate[M, N, A]((i, j) => nested(i)(j))

  extension [N <: Int, A](v: CC[N, A])
    /** Generalized outer product for different input vector types and output type. */
    inline def outer[M <: Int, B, C](w: CC[M, B],
                                     inline combine: (A, B) => C): MatrixType[N, M, C] =
      matrixFactory.tabulate[N, M, C]((i, j) => combine(v(i), w(j)))
    /** Interpret the flat vector in a Matrix with given dimension. */
    inline def reshape[O <: Int, P <: Int](using O*P =:= N): MatrixType[O, P, A]
    /** Interpret the vector as a single-row matrix. */
    inline def asRow: MatrixType[1, N, A]
    /** Interpret the vector as a single-column matrix. */
    inline def asColumn: MatrixType[N, 1, A]
