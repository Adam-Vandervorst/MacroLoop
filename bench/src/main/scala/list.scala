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


@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Fork(2, warmups = 2)
@State(Scope.Benchmark)
class ListReArrange:
  def rearrange1[A](data: List[A], indexes: List[Int]): List[A] =
    import scala.collection.immutable.Queue

    val (newIndexes, remainingElements) = data.lazyZip(indexes).partitionMap {
      case (a, idx) =>
        Either.cond(
          test = idx == 0,
          right = a,
          left = idx -> a
        )
    }

    val newIndexesMap = newIndexes.toMap

    List.unfold((1 -> Queue.from(remainingElements))) {
      case (idx, queue) =>
        val attemptUsingMap = newIndexesMap.get(key = idx).map { a =>
          a -> ((idx + 1) -> queue)
        }

        val attemptUsingQueue = queue.dequeueOption.map {
          case (a, q) =>
            a -> ((idx + 1) -> q)
        }

        attemptUsingMap orElse attemptUsingQueue
    }
  end rearrange1

  def rearrange2[A](data: List[A], indices: List[Int]): List[A] =
    val (mapping, remaining) = (data lazyZip indices).foldRight(Map.empty[Int, A], List.empty[A]) {
      case ((a, 0), (m, rem)) => (m, a :: rem)
      case ((a, i), (m, rem)) => (m + (i -> a), rem)
    }

    List.unfold(1 -> remaining) {
      case (i@mapping(a), queue) => Some(a -> (i + 1, queue))
      case (i, head :: todo) => Some(head -> (i + 1, todo))
      case _ => None
    }
  end rearrange2

  def rearrange3[A](data: List[A], indices: List[Int]): List[A] =
    val mapping = collection.mutable.LongMap.empty[A]
    var remaining: List[A] = Nil

    for (a, i) <- (data lazyZip indices) do
      if i == 0 then remaining = a :: remaining
      else mapping.update(i, a)

    var res: List[A] = Nil
    var i = data.length
    while i > 0 do
      val stored = mapping.getOrElse(i, {
        val h = remaining.head
        remaining = remaining.tail
        h
      })
      res = stored :: res
      i -= 1
    res
  end rearrange3

  def rearrange4[A](initial: Vector[A], indexes: Vector[Int]): Vector[A] = {
    val indexesFiltered = indexes.zip(1 to indexes.size).filter(_._1 > 0)
    val initZipped = initial.zip(1 to initial.size)
    val notIndexedInit = initZipped.filterNot(init => indexesFiltered.exists(_._2 == init._2))
    indexesFiltered
      .sortBy(_._2)
      .foldLeft(notIndexedInit)((prev, index) =>
        prev.patch(index._1 - 1, initZipped.filter(index._2 == _._2), 0)
      ).map(_._1)
  }

  @Benchmark
  def _rearrange1 = rearrange1(List('a', 'b', 'c', 'e', 'd'), List(0, 0, 5, 0, 2))
  @Benchmark
  def _rearrange2 = rearrange2(List('a', 'b', 'c', 'e', 'd'), List(0, 0, 5, 0, 2))
  @Benchmark
  def _rearrange3 = rearrange3(List('a', 'b', 'c', 'e', 'd'), List(0, 0, 5, 0, 2))
  @Benchmark
  def _rearrange4 = rearrange4(Vector('a', 'b', 'c', 'e', 'd'), Vector(0, 0, 5, 0, 2))

end ListReArrange


@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 5, time = 500, timeUnit = TimeUnit.MILLISECONDS)
@Fork(2, warmups = 2)
@State(Scope.Benchmark)
class ListTabulate:
  val n = 10
  def f(x: Int) = -x

  @Benchmark
  def _tabulate1 =
    val b = List.newBuilder[Int]
    b.sizeHint(n)
    var i = 0
    while i < n do
      b += f(i)
      i += 1
    b.result()

  @Benchmark
  def _tabulate2 =
    var res: List[Int] = Nil
    var i = n - 1
    while i >= 0 do
      res = f(i)::res
      i -= 1
    res
end ListTabulate
