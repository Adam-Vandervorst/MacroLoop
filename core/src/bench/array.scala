package macroloop.bench

import macroloop.*

import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.infra.Blackhole


@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 250, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 250, timeUnit = TimeUnit.MILLISECONDS)
@Fork(2, warmups = 2)
@State(Scope.Benchmark)
class ArraySum:
  private val ar = Array.range(1, 10001)

  @Benchmark
  def baseline_const =
    50005000

  @Benchmark
  def default_sum =
    ar.sum

  @Benchmark
  def default_for =
    var k = 0
    for i <- ar do
      k += i
    k

  @Benchmark
  def default_while_it =
    var k = 0
    val it = ar.iterator
    while it.hasNext do
      k += it.next()
    k

  @Benchmark
  def default_while_index =
    var k = 0
    var i = 0
    while i < ar.length do
      k += ar(i)
      i += 1
    k

  @Benchmark
  def macro_while_index =
    var k = 0
    ArrayIndex.forEach(ar)(k += _)
    k

  @Benchmark
  def macro_while_index_unrolled_4 =
    var k = 0
    ArrayIndex.forEachUnrolledN(4)(ar)(k += _)
    k

  @Benchmark
  def macro_while_index_unrolled_32 =
    var k = 0
    ArrayIndex.forEachUnrolledN(32)(ar)(k += _)
    k

  @Benchmark
  def port_while_index =
    var k = 0
    port.ArrayIndex.forEach[Int, Unit](ar.asInstanceOf, k += _)
    k
end ArraySum


@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 250, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 250, timeUnit = TimeUnit.MILLISECONDS)
@Fork(2, warmups = 2)
@State(Scope.Benchmark)
class ArraySumJava:
  private val ar = Array.range(1, 10001).map(Int.box)

  @Benchmark
  def baseline_const =
    50005000

  @Benchmark
  def default_sum =
    ar.sum(using Numeric.IntIsIntegral.asInstanceOf)

  @Benchmark
  def default_for =
    var k = 0
    for i <- ar do
      k += i
    k

  @Benchmark
  def default_while_it =
    var k = 0
    val it = ar.iterator
    while it.hasNext do
      k += it.next()
    k

  @Benchmark
  def default_while_index =
    var k = 0
    var i = 0
    while i < ar.length do
      k += ar(i)
      i += 1
    k

  @Benchmark
  def macro_while_index =
    var k = 0
    ArrayIndex.forEach(ar)(k += _)
    k

  @Benchmark
  def macro_while_index_unrolled_4 =
    var k = 0
    ArrayIndex.forEachUnrolledN(4)(ar)(k += _)
    k

  @Benchmark
  def macro_while_index_unrolled_32 =
    var k = 0
    ArrayIndex.forEachUnrolledN(32)(ar)(k += _)
    k

  @Benchmark
  def port_while_index =
    var k = 0
    port.ArrayIndex.forEach[java.lang.Integer, Unit](ar, k += _)
    k
end ArraySumJava


@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 250, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 250, timeUnit = TimeUnit.MILLISECONDS)
@Fork(2, warmups = 2)
@State(Scope.Benchmark)
class ArrayForall:
  @Param(Array("10", "990", "-1"))
  var matching: Int = _

  private val ar = Array.range(1, 1001)

  @Benchmark
  def baseline_const =
    matching == -1

  @Benchmark
  def default_forall =
    ar.forall(_ != matching)

  @Benchmark
  def default_for: Boolean =
    for i <- ar do
      if i == matching then
        return false
    true

  @Benchmark
  def default_while_it_return: Boolean =
    val it = ar.iterator
    while it.hasNext do
      if it.next() == matching then
        return false
    true

  @Benchmark
  def macro_while_index_exception =
    ArrayIndex.forallException(ar)(_ != matching)

  @Benchmark
  def macro_while_index_condition =
    ArrayIndex.forallCondition(ar)(_ != matching)

  @Benchmark
  def port_while_index =
    var k = 0
    port.ArrayIndex.forall[Int](ar.asInstanceOf, _ != matching)
    k
end ArrayForall


@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 250, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 250, timeUnit = TimeUnit.MILLISECONDS)
@Fork(2, warmups = 2)
@State(Scope.Benchmark)
class ArrayForallMod:
  @Param(Array("57", "1977", "1000"))
  var matching: Int = _

  private val ar: Array[Int] = Array.tabulate(1000)(i => 2*i + i % 20)

  @Benchmark
  def baseline_const =
    matching == -1

  @Benchmark
  def default_forall =
    ar.forall(_ != matching)

  @Benchmark
  def default_for: Boolean =
    for i <- ar do
      if i == matching then
        return false
    true

  @Benchmark
  def default_while_it_return: Boolean =
    val it = ar.iterator
    while it.hasNext do
      if it.next() == matching then
        return false
    true

  @Benchmark
  def macro_while_index_exception =
    ArrayIndex.forallException(ar)(_ != matching)

  @Benchmark
  def macro_while_index_condition =
    ArrayIndex.forallCondition(ar)(_ != matching)

  @Benchmark
  def port_while_index =
    port.ArrayIndex.forall[Int](ar.asInstanceOf, _ != matching)
end ArrayForallMod
