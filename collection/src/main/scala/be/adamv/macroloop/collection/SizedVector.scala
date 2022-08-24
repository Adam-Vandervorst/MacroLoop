package be.adamv.macroloop.collection

import be.adamv.macroloop.{ArrayIndex, IntRange, SizedArrayIndex}

import scala.compiletime.constValue
import scala.compiletime.ops.int.*


class SizedVector[N <: Int, A](val data: Array[A]):
  inline def length: N = constValue

  inline def apply(inline i: Int): A = data(i)
  inline def update(inline i: Int, inline v: A): Unit = data(i) = v

  inline def reshape[O <: Int, P <: Int](using O*P =:= N): Matrix[O, P, A] =
    Matrix.tabulate((i, j) => this(i*constValue[P] + j))
  inline def chunked[DN <: Int](using N % DN =:= 0): SizedVector[DN, SizedVector[N/DN, A]] =
    SizedVector.tabulate(di => SizedVector.tabulate(i => this(di*constValue[N/DN] + i)))
  inline def slice[I1 <: Int, I2 <: Int]: SizedVector[I2 - I1, A] =
    SizedVector(data.slice(constValue[I1], constValue[I2]))

  inline def forEach(inline f: A => Unit): Unit = ArrayIndex.forEach(data)(f)
  inline def map[B](inline f: A => B): SizedVector[N, B] =
    SizedVector(SizedArrayIndex.mapUnrolled(data, constValue[N])(f))
  inline def flatMap[M <: Int, B](inline f: A => SizedVector[M, B]): SizedVector[M*N, B] =
    SizedVector(SizedArrayIndex.flatMapFullyUnrolled(data, constValue[N])(f(_).data, constValue[M]))

  inline def convolve[M <: Int, B, C](kernel: SizedVector[M, B],
                                      inline combine: (A, B) => C, inline add: (C, C) => C, inline zero: C): SizedVector[N, C] =
    val c = kernel.length/2

    val result: Array[C] = SizedArrayIndex.ofSize[N, C]
    IntRange.forEach(0, constValue[N], 1)(i => result(i) = zero)

    for i <- 0 until length
        k <- 0 until kernel.length do
      val ii = i + (k - c)

      if ii >= 0 && ii < length then
        result(i) = add(result(i), combine(this(ii), kernel(k)))
    SizedVector(result)

  inline def kronecker[M <: Int, B, C](that: SizedVector[M, B],
                                       inline combine: (A, B) => C): SizedVector[M*N, C] =
    // workaround weird inlining "Term-dependent types are experimental" bug
    def inner(a: A) = that.map(combine(a, _))
    this.flatMap(inner)

  inline def hadamard[B, C](that: SizedVector[N, B],
                            inline combine: (A, B) => C): SizedVector[N, C] =

    val cdata: Array[C] = SizedArrayIndex.ofSize[N, C]
    IntRange.forEach(0, constValue[N], 1)(i => cdata(i) = combine(this.data(i), that.data(i)))
    SizedVector(cdata)

  inline def inner[B, C](that: SizedVector[N, B],
                         inline combine: (A, B) => C, inline add: (C, C) => C, inline zero: C): C =
    var result = zero

    for i <- 0 until length do
      result = add(result, combine(this(i), that(i)))
    result

  inline def outer[M <: Int, B, C](that: SizedVector[M, B],
                                             inline combine: (A, B) => C): Matrix[N, M, C] =
    Matrix.tabulate[N, M, C]((i, j) => combine(this(i), that(j)))

  inline def show: String = data.mkString(",")

  override def equals(that: Any): Boolean = that match
    case that: SizedVector[n, _] => this.data sameElements that.data
    case _ => false


object SizedVector:
  inline def apply[N <: Int, A](data: Array[A]) = new SizedVector[N, A](data.asInstanceOf)

  extension [A](v: SizedVector[1, A])
    inline def singleElement: A = v(0)
  inline def asSingleElement[A](a: A): SizedVector[1, A] =
    val data: Array[A] = SizedArrayIndex.ofSize[1, A]
    data(0) = a
    SizedVector(data)

  inline def from[N <: Int, A](as: IterableOnce[A]): SizedVector[N, A] =
    val data: Array[A] = SizedArrayIndex.ofSize[N, A]
    val it = as.iterator
    IntRange.forEach(0, constValue[N], 1)(i => data(i) = it.next())
    SizedVector(data)

  inline def fromSparse[N <: Int, A](pf: PartialFunction[Int, A]): SizedVector[N, Option[A]] =
    SizedVector.tabulate(pf.unapply)

  inline def tabulate[N <: Int, A](inline f: Int => A): SizedVector[N, A] =
    val data: Array[A] = SizedArrayIndex.ofSize[N, A]
    IntRange.forEach(0, constValue[N], 1)(i => data(i) = f(i))
    SizedVector(data)

  inline def fill[N <: Int, A](v: A): SizedVector[N, A] = SizedVector.tabulate(_ => v)

  extension [M <: Int, N <: Int, A](nested: SizedVector[M, SizedVector[N, A]])
    inline def toMatrix: Matrix[M, N, A] =
      Matrix.tabulate[M, N, A]((i, j) => nested(i)(j))
  
  extension [N <: Int, A](v: SizedVector[N, A])
    inline def asRow: Matrix[1, N, A] = Matrix(v.data.clone())
    inline def asColumn: Matrix[N, 1, A] = Matrix(v.data.clone())
