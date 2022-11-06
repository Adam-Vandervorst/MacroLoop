package be.adamv.macroloop.bench

import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.infra.Blackhole

import be.adamv.macroloop.*


@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 250, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(2, warmups = 2)
@State(Scope.Benchmark)
class ForallTripleIterable:
  @Param(Array("small-false", "small-contains", "large-false", "large-front", "large-back"))
  var mode: String = _

  protected var matching: Int = _
  protected var list: List[Char] = _
  protected var array: Array[Int] = _
  protected var range: Range.Inclusive = _

  @Setup
  def setup =
    mode match
      case "small-false" =>
        matching = 101
        list = List('a', 'b', 'c')
        array = Array.fill(2)(10)
        range = Range.inclusive(1, 10)
      case "small-contains" =>
        matching = 50
        list = List('a', 'b', 'c')
        array = Array.fill(2)(10)
        range = Range.inclusive(1, 10)
      case "large-false" =>
        matching = 1001
        list = ('a' to 'z').toList
        array = Array.fill(10)(10)
        range = Range.inclusive(1, 100)
      case "large-front" =>
        matching = 50
        list = ('a' to 'z').toList
        array = Array.fill(10)(10)
        range = Range.inclusive(1, 100)
      case "large-back" =>
        matching = 950
        list = ('a' to 'z').toList
        array = Array.fill(10)(10)
        range = Range.inclusive(1, 100)

  @Benchmark
  def baseline = false

  @Benchmark
  def nested =
    list.forall(c => array.forall(i => range.forall(j => i*j <= matching)))

  @Benchmark
  def iteratorFlatMap =
    list.iterator.flatMap(c => array.iterator.flatMap(i => range.iterator.map(j => (c, i, j))))
      .forall((c, i, j) => i*j <= matching)

  @Benchmark
  def macroException =
    IterableIt.forallExceptionCart[(Char, Int, Int)]((list, array, range))(t => t._2*t._3 <= matching)
end ForallTripleIterable
