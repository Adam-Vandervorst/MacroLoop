package be.adamv.macroloop.bench

import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.infra.Blackhole

import be.adamv.macroloop.*


@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 2, time = 250, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 250, timeUnit = TimeUnit.MILLISECONDS)
@Fork(2, warmups = 2)
@State(Scope.Benchmark)
class Fact:
  inline val fact_1 = 12 + 1

  @Benchmark
  def baseline_const =
    479001600

  @Benchmark
  def default_for =
    var k = 1
    for i <- 1 until fact_1 do
      k *= i
    k

  @Benchmark
  def default_while =
    var k = 1
    var i = 1
    while i < fact_1 do
      k *= i
      i += 1
    k

  @Benchmark
  def macro_while =
    var k = 1
    IntRange.forEach(1, fact_1, 1)(k *= _)
    k

  @Benchmark
  def macro_unrolled =
      var k = 1
      IntRange.forEachUnrolled(1, fact_1, 1)(k *= _)
      k
end Fact
