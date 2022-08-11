package macroloop.collection

import scala.reflect.ClassTag
import compiletime.constValue
import compiletime.ops.int.*


class SizedVector[N <: Int, A : ClassTag](val data: Array[A]):
  inline def length: N = constValue

  inline def apply(inline i: Int): A = data(i)
  inline def update(inline i: Int, inline v: A): Unit = data(i) = v

  inline def reshape[O <: Int, P <: Int](using O*P =:= N): Matrix[O, P, A] =
    Matrix.tabulate((i, j) => this(i*constValue[P] + j))
  inline def slice[I1 <: Int, I2 <: Int]: SizedVector[I2 - I1, A] =
    SizedVector(data.slice(constValue[I1], constValue[I2]))

  inline def map[B : ClassTag](inline f: A => B): SizedVector[N, B] = SizedVector(data.map(f))
  inline def flatMap[M <: Int, B : ClassTag](inline f: A => SizedVector[M, B]): SizedVector[M*N, B] = SizedVector(data.flatMap(f(_).data))

  inline def convolve[M <: Int, B, C : ClassTag](kernel: SizedVector[M, B],
                                          inline combine: (A, B) => C, inline add: (C, C) => C, inline zero: C): SizedVector[N, C] =
    val c = kernel.length/2

    val result = Array.fill(length)(zero)

    for i <- 0 until length
        k <- 0 until kernel.length do
      val ii = i + (k - c)

      if ii >= 0 && ii < length then
        result(i) = add(result(i), combine(this(ii), kernel(k)))
    SizedVector(result)

  inline def kronecker[M <: Int, B, C : ClassTag](that: SizedVector[M, B],
                                                  inline combine: (A, B) => C): SizedVector[M*N, C] =
    this.flatMap(a => that.map(b => combine(a, b)))

  inline def hadamard[B, C : ClassTag](that: SizedVector[N, B],
                                       inline combine: (A, B) => C): SizedVector[N, C] =
    val cdata = new Array[C](length)
    var i = 0
    while i < length do
      cdata(i) = combine(this.data(i), that.data(i))
      i += 1
    SizedVector(cdata)

  inline def inner[B, C: ClassTag](that: SizedVector[N, B],
                                   inline combine: (A, B) => C, inline add: (C, C) => C, inline zero: C): C =
    var result = zero

    for i <- 0 until length do
      result = add(result, combine(this (i), that(i)))
    result

  inline def outer[M <: Int, B, C: ClassTag](that: SizedVector[M, B],
                                             inline combine: (A, B) => C): Matrix[N, M, C] =
    Matrix.tabulate[N, M, C]((i, j) => combine(this(i), that(j)))

  inline def show: String = data.mkString(",")

  override def equals(that: Any): Boolean = that match
    case that: SizedVector[n, _] => this.data sameElements that.data
    case _ => false

extension [M <: Int, N <: Int, A : ClassTag](nested: SizedVector[M, SizedVector[N, A]])
  inline def toMatrix: Matrix[M, N, A] =
    Matrix.tabulate[M, N, A]((i, j) => nested(i)(j))

extension [N <: Int, A : ClassTag](v: SizedVector[N, A])
  inline def asRow: Matrix[1, N, A] = Matrix(v.data.clone())
  inline def asColumn: Matrix[N, 1, A] = Matrix(v.data.clone())


object SizedVector:
  extension [A : ClassTag](v: SizedVector[1, A])
    inline def singleElement: A = v(0)
  inline def asSingleElement[A : ClassTag](a: A): SizedVector[1, A] = SizedVector(Array(a))

  inline def from[N <: Int, A : ClassTag](as: IterableOnce[A]): SizedVector[N, A] = SizedVector(Array.from(as))

  inline def tabulate[N <: Int, A : ClassTag](inline f: Int => A): SizedVector[N, A] =
    val n = constValue[N]
    val data = new Array[A](n)
    var i = 0
    while i < n do
      data(i) = f(i)
      i += 1
    SizedVector(data)

  inline def fill[N <: Int, A : ClassTag](v: A): SizedVector[N, A] = SizedVector.tabulate(_ => v)
