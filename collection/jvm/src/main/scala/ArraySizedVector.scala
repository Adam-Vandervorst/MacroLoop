package be.adamv.macroloop.collection

import be.adamv.macroloop.{ArrayIndex, IntRange, SizedArrayIndex}

import scala.compiletime.constValue
import scala.compiletime.ops.int.*


/**
 * Array-backed Sized Vector imlementation with mutable entries.
 * @tparam N Elements
 * @tparam A Element-type
 */
abstract class ArraySizedVector[N <: Int, A] extends SizedVectorOps[N, A, ArraySizedVector]:
  override inline def factory: ArraySizedVector.type = ArraySizedVector
  val data: Array[A]

  /** Get the element in position i. */
  inline def apply(inline i: Int): A = data(i)
  /** Set the element in position i to v. */
  inline def update(inline i: Int, inline v: A): Unit = data(i) = v

  /** An iterator over all elements. */
  inline def iterator: Iterator[A] = data.iterator

  /** Divide up this vector in DN chunks, creating a vector of vectors. */
  inline def chunked[DN <: Int](using N % DN =:= 0): ArraySizedVector[DN, ArraySizedVector[N/DN, A]] =
    val isize = constValue[N/DN]
    factory.tabulate { i =>
      val idata = SizedArrayIndex.ofSize[N / DN, A]
      Array.copy(data, i * isize, idata, 0, isize)
      factory.wrap(idata)
    }
  /** Take an I1 to I2 slice. */
  inline def slice[I1 <: Int, I2 <: Int]: ArraySizedVector[I2 - I1, A] =
    factory.wrap(data.slice(constValue[I1], constValue[I2]))

  /** Seq of all elements. */
  inline def toSeq: Seq[A] = collection.immutable.ArraySeq.unsafeWrapArray(data)

  /** Perform a side-effect to each element. */
  inline def forEach(inline f: A => Unit): Unit = ArrayIndex.forEach(data)(f)
  /** Same size vector with its elements transformed by f. */
  inline def map[B](inline f: A => B): ArraySizedVector[N, B] =
    factory.wrap(SizedArrayIndex.mapForSize(data, constValue[N])(f))
  /** Each element gets expanded into a sub-vector by f. */
  inline def flatMap[M <: Int, B](inline f: A => ArraySizedVector[M, B]): ArraySizedVector[M*N, B] =
    factory.wrap(SizedArrayIndex.flatMapFullyUnrolled(data, constValue[N])(f(_).data, constValue[M]))

  /** Move kernel over the vector, combining all overlapping pairs, and accumulating them with add and zero into a new entry. */
//  inline def convolve[M <: Int, B, C](kernel: ArraySizedVector[M, B],
//                                      inline combine: (A, B) => C, inline add: (C, C) => C, inline zero: C): ArraySizedVector[N, C] =
//    val c = kernel.length/2
//
//    val result: Array[C] = SizedArrayIndex.ofSize[N, C]
//    IntRange.forEach(constValue[N])(i => result(i) = zero)
//
//    for i <- 0 until length
//        k <- 0 until kernel.length do
//      val ii = i + (k - c)
//
//      if ii >= 0 && ii < length then
//        result(i) = add(result(i), combine(this(ii), kernel(k)))
//    factory.wrap(result)

  /** Generalized kronecker (outer) product for different input vector types and output type, flattened. */
//  inline def kronecker[M <: Int, B, C](that: ArraySizedVector[M, B],
//                                       inline combine: (A, B) => C): ArraySizedVector[M*N, C] =
//    val cdata = SizedArrayIndex.ofSize[M*N, C]
//    var i = 0
//    while i < length do
//      val a = data(i)
//      val offset = i*constValue[M]
//      var j = 0
//      while j < that.length do
//        cdata(offset + j) = combine(a, that.data(j))
//        j += 1
//      i += 1
//    factory.wrap(cdata)

  /** Generalized element-wise product for different input vector types and output type. */
//  inline def elementwise[B, C](that: ArraySizedVector[N, B],
//                               inline combine: (A, B) => C): ArraySizedVector[N, C] =
//
//    val cdata: Array[C] = SizedArrayIndex.ofSize[N, C]
//    IntRange.forEach(constValue[N])(i => cdata(i) = combine(this.data(i), that.data(i)))
//    factory.wrap(cdata)

  /** Generalized inner product for different input vector types and output type. */
//  inline def inner[B, C](that: ArraySizedVector[N, B],
//                         inline combine: (A, B) => C, inline add: (C, C) => C, inline zero: C): C =
//    var result = zero
//    IntRange.forEach(length)(i => result = add(result, combine(this(i), that(i))))
//    result

  // FIXME standard toString is not-inlineable, so it doesn't allow a dimension-based representation
  /** Pretty string, do not rely on the precise output. */
  inline def show: String = data.mkString(",")

  /** Equality on vectors. */
  override def equals(that: Any): Boolean = that match
    case that: ArraySizedVector[n, _] => this.data sameElements that.data
    case _ => false


object ArraySizedVector extends SizedVectorFactory[Array, ArraySizedVector]:
  import be.adamv.macroloop.collection.macros.concreteVectorImpl
  transparent inline def apply[Tup <: Tuple](inline elements: Tup): ArraySizedVector[Tuple.Size[Tup], Tuple.Union[Tup]] =
    ${ concreteVectorImpl[Tuple.Size[Tup], Tuple.Union[Tup]]('elements) }

  /** Size and data array to SizedVector. */
  inline def wrap[N <: Int, A](inline initial: Array[A]): ArraySizedVector[N, A] =
    assert(constValue[N] == initial.length) // NOTE not compiletime
    new:
      override val data: Array[A] = initial

//  extension [A](v: ArraySizedVector[1, A])
//    inline def singleElement: A = v(0)
//  inline def asSingleElement[A](a: A): ArraySizedVector[1, A] = apply(Tuple1(a))

  /** Take N elements from an iterator to construct a SizedVector. */
  inline def from[N <: Int, A](as: IterableOnce[A]): ArraySizedVector[N, A] =
    val data: Array[A] = SizedArrayIndex.ofSize[N, A]
    val written = as.iterator.copyToArray(data, 0, constValue[N])
    assert(written == constValue[N])
    wrap(data)

  // TODO this probably not the expected behavior
  // TODO have a sparse class too?
  /** Like tabulate but for partial functions. */
//  inline def fromSparse[N <: Int, A](pf: PartialFunction[Int, A]): ArraySizedVector[N, Option[A]] =
//    tabulate(pf.unapply)

  /** Fill a SizedVector with elements dependent on their integer position. */
  inline def tabulate[N <: Int, A](inline f: Int => A): ArraySizedVector[N, A] =
    val data: Array[A] = SizedArrayIndex.ofSize[N, A]
    IntRange.forEach(constValue[N])(i => data(i) = f(i))
    wrap(data)

  /** Fill a vector with a certain element. */
//  inline def fill[N <: Int, A](v: A): ArraySizedVector[N, A] = tabulate(_ => v)

//  extension [M <: Int, N <: Int, A](nested: ArraySizedVector[M, ArraySizedVector[N, A]])
//    // TODO use copy primitives
//    /** Interpreter the inner vectors as rows of a matrix. */
//    inline def toMatrix: Matrix[M, N, A] =
//      Matrix.tabulate[M, N, A]((i, j) => nested(i)(j))
//
//  extension [N <: Int, A](v: ArraySizedVector[N, A])
//    /** Generalized outer product for different input vector types and output type. */
//    inline def outer[M <: Int, B, C](w: ArraySizedVector[M, B],
//                                     inline combine: (A, B) => C): Matrix[N, M, C] =
//      Matrix.tabulate[N, M, C]((i, j) => combine(v(i), w(j)))
//    /** Interpret the flat vector in a Matrix with given dimension. */
//    inline def reshape[O <: Int, P <: Int](using O*P =:= N): Matrix[O, P, A] = Matrix.wrap(v.data.clone())
//    /** Interpret the vector as a single-row matrix. */
//    inline def asRow: Matrix[1, N, A] = Matrix.wrap(v.data.clone())
//    /** Interpret the vector as a single-column matrix. */
//    inline def asColumn: Matrix[N, 1, A] = Matrix.wrap(v.data.clone())
