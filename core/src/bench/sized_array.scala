package be.adamv.macroloop.bench

import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.infra.Blackhole

import be.adamv.macroloop.*


@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 250, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 250, timeUnit = TimeUnit.MILLISECONDS)
@Fork(2, warmups = 2)
@State(Scope.Benchmark)
class SmallArrayMap:
  private val ar = Array.range(1, 5)

  @Benchmark
  def array_creation =
    Array(2, 3, 4, 5)

  @Benchmark
  def default_map =
    ar.map(_ + 1)

  @Benchmark
  def macro_sized_map_unrolled =
    SizedArrayIndex.mapUnrolled(ar, 4)(_ + 1)
end SmallArrayMap

