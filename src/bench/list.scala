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
class ListRemoveFirst:
  // thanks to @jumman_abedin for bringing this up
  // thanks to @velvetbaldmime @BalmungSan @zzyzzyxx and @SethTisue for the respective solutions
  @Param(Array("small-false", "small-contains", "large-false", "large-front", "large-back"))
  var mode: String = _

  protected var matching: Int = _
  protected var ar: List[Int] = _

  @Setup
  def setup =
    mode match
      case "small-false" =>
        matching = 11
        ar = List.range(1, 10)
      case "small-contains" =>
        matching = 5
        ar = List.range(1, 10)
      case "large-false" =>
        matching = 1001
        ar = List.range(1, 1000)
      case "large-front" =>
        matching = 50
        ar = List.range(1, 1000)
      case "large-back" =>
        matching = 950
        ar = List.range(1, 1000)

  @Benchmark
  def baseline_const =
    ar

  @Benchmark
  def splitAt_concat =
    val (b, a) = ar.splitAt(ar.indexOf(matching))
    if a.headOption.contains(matching) then b ++ a.tail
    else ar

  @Benchmark
  def tailrec_acc =
    @annotation.tailrec
    def loop(remaining: List[Int], acc: List[Int]): List[Int] =
      remaining match
        case head :: tail =>
          if (head == matching) acc reverse_::: tail
          else loop(tail, head :: acc)
        case Nil =>
          ar
    end loop
    loop(remaining = ar, acc = List.empty)

  @Benchmark
  def iterator_takewhile_concat =
    val it = ar.iterator
    (it.takeWhile(_ != matching) ++ it).toList

  @Benchmark
  def span_concat =
    // fails if not contained
    val (initial, rest) = ar.span(_ != matching)
    initial ++ rest.tail

  @Benchmark
  def mutating_filter =
    var todo = true
    ar.filter(e => if (todo && e == matching) { todo = false; false } else true)
end ListRemoveFirst

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 250, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 250, timeUnit = TimeUnit.MILLISECONDS)
@Fork(2, warmups = 2)
@State(Scope.Benchmark)
class ListIndexOf:
  @Param(Array("small-false", "small-contains", "large-false", "large-front", "large-back"))
  var mode: String = _

  protected var matching: Int = _
  protected var ar: List[Int] = _

  @Setup
  def setup =
    mode match
      case "small-false" =>
        matching = 11
        ar = List.range(1, 10)
      case "small-contains" =>
        matching = 5
        ar = List.range(1, 10)
      case "large-false" =>
        matching = 1001
        ar = List.range(1, 1000)
      case "large-front" =>
        matching = 50
        ar = List.range(1, 1000)
      case "large-back" =>
        matching = 950
        ar = List.range(1, 1000)

  @Benchmark
  def baseline_const = -1

  @Benchmark
  def base = ar.indexOf(matching)

  @Benchmark
  def it = ar.iterator.indexOf(matching)

  @Benchmark
  def while_mut =
    var t = ar
    var i = 0
    while t != Nil do
      if t.head == matching then
        t = Nil
        i = -1
      else
        i += 1
        t = t.tail
    i
end ListIndexOf
