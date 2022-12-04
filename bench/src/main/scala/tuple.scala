package be.adamv.macroloop.bench

import be.adamv.macroloop.*
import be.adamv.macroloop.collection.*

import org.openjdk.jmh.annotations.*

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.infra.Blackhole


@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Fork(2, warmups = 2)
@State(Scope.Benchmark)
class TupleAlternativesMap:
  @Benchmark
  def tuple_default(bh: Blackhole) =
    bh.consume(("foo", "bar").map[[_] =>> Int]([X] => (x: X) => x match { case s: String => s.length }))

  @Benchmark
  def tuple_macro(bh: Blackhole) =
    bh.consume(ConstantTuple.mapBoundedUnrolled("foo", "bar")((s: String) => s.length))

  @Benchmark
  def vectorN(bh: Blackhole) =
    bh.consume(VectorNArray("foo", "bar").map(_.length))

  @Benchmark
  def array(bh: Blackhole) =
    bh.consume(Array("foo", "bar").map(_.length))

  case class Pair[X](l: X, r: X):
    def map[Y](f: X => Y): Pair[Y] = Pair(f(l), f(r))

  @Benchmark
  def custom_pair(bh: Blackhole) =
    bh.consume(Pair("foo", "bar").map(_.length))

  @Benchmark
  def list(bh: Blackhole) =
    bh.consume(List("foo", "bar").map(_.length))
end TupleAlternativesMap
