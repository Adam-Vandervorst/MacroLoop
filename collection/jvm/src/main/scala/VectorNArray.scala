package be.adamv.macroloop.collection

import be.adamv.macroloop.{ArrayIndex, IntRange, SizedArrayIndex}

import scala.compiletime.constValue
import scala.compiletime.ops.int.*


/**
 * Array-backed Sized Vector imlementation with mutable entries.
 * @tparam N Elements
 * @tparam A Element-type
 */
final class VectorNArray[N <: Int, A](final val data: Array[A]) extends AnyVal with VectorNOps[N, A, VectorNArray]:
  override inline def factory: VectorNArray.type = VectorNArray

  /** Get the element in position i. */
  inline def apply(inline i: Int): A = data(i)
  /** Set the element in position i to v. */
  inline def update(inline i: Int, inline v: A): Unit = data(i) = v

  /** An iterator over all elements. */
  inline def iterator: Iterator[A] = data.iterator

  /** Divide up this vector in DN chunks, creating a vector of vectors. */
  inline def chunked[DN <: Int](using N % DN =:= 0): VectorNArray[DN, VectorNArray[N/DN, A]] =
    val isize = constValue[N/DN]
    factory.tabulate { i =>
      val idata = SizedArrayIndex.ofSize[N / DN, A]
      Array.copy(data, i * isize, idata, 0, isize)
      factory.wrap(idata)
    }
  /** Take an I1 to I2 slice. */
  inline def slice[I1 <: Int, I2 <: Int]: VectorNArray[I2 - I1, A] =
    factory.wrap(data.slice(constValue[I1], constValue[I2]))

  /** Seq of all elements. */
  inline def toSeq: Seq[A] = collection.immutable.ArraySeq.unsafeWrapArray(data)

  /** Perform a side-effect to each element. */
  inline def forEach(inline f: A => Unit): Unit = ArrayIndex.forEach(data)(f)
  /** Same size vector with its elements transformed by f. */
  inline def map[B](inline f: A => B): VectorNArray[N, B] =
    factory.wrap(SizedArrayIndex.mapForSize(data, constValue[N])(f))
  /** Each element gets expanded into a sub-vector by f. */
  inline def flatMap[M <: Int, B](inline f: A => VectorNArray[M, B]): VectorNArray[M*N, B] =
    factory.wrap(SizedArrayIndex.flatMapFullyUnrolled(data, constValue[N])(f(_).data, constValue[M]))

  /** Generalized kronecker product for different input vector types and output type, flattened. */
  inline def kronecker[M <: Int, B, C](that: VectorNArray[M, B],
                                       inline combine: (A, B) => C): VectorNArray[M*N, C] =
    val cdata = SizedArrayIndex.ofSize[M*N, C]
    var i = 0
    while i < length do
      val a = data(i)
      val offset = i*constValue[M]
      var j = 0
      while j < that.length do
        cdata(offset + j) = combine(a, that.data(j))
        j += 1
      i += 1
    factory.wrap(cdata)

  // FIXME standard toString is not-inlineable, so it doesn't allow a dimension-based representation
  /** Pretty string, do not rely on the precise output. */
  inline def show: String = data.mkString(",")

  /** Equality on vectors. */
  override def equals(that: Any): Boolean = that match
    case that: VectorNArray[n, _] => this.data sameElements that.data
    case _ => false


object VectorNArray extends VectorNFactory[Array, VectorNArray]:
  type MatrixType[M <: Int, N <: Int, A] = MatrixMNArray[M, N, A]
  inline def matrixFactory: MatrixMNArray.type = MatrixMNArray

  transparent inline def apply[Tup <: Tuple](inline elements: Tup): VectorNArray[Tuple.Size[Tup], Tuple.Union[Tup]] =
    ${ macros.concreteVectorImpl[Tuple.Size[Tup], Tuple.Union[Tup]]('elements) }

  /** Size and data array to SizedVector. */
  inline def wrap[N <: Int, A](inline initial: Array[A]): VectorNArray[N, A] =
    assert(constValue[N] == initial.length) // NOTE not compiletime
    new VectorNArray(initial)

  /** Take N elements from an iterator to construct a SizedVector. */
  inline def from[N <: Int, A](as: IterableOnce[A]): VectorNArray[N, A] =
    val data: Array[A] = SizedArrayIndex.ofSize[N, A]
    val written = as.iterator.copyToArray(data, 0, constValue[N])
    assert(written == constValue[N])
    new VectorNArray(data)

  /** Fill a SizedVector with elements dependent on their integer position. */
  inline def tabulate[N <: Int, A](inline f: Int => A): VectorNArray[N, A] =
    val data: Array[A] = SizedArrayIndex.ofSize[N, A]
    IntRange.forEach(constValue[N])(i => data(i) = f(i))
    new VectorNArray(data)

  extension [N <: Int, A](v: VectorNArray[N, A])
    /** Interpret the flat vector in a Matrix with given dimension. */
    inline def reshape[O <: Int, P <: Int](using O*P =:= N): MatrixMNArray[O, P, A] = matrixFactory.wrap(v.data.clone())
    /** Interpret the vector as a single-row matrix. */
    inline def asRow: MatrixMNArray[1, N, A] = matrixFactory.wrap(v.data.clone())
    /** Interpret the vector as a single-column matrix. */
    inline def asColumn: MatrixMNArray[N, 1, A] = matrixFactory.wrap(v.data.clone())
