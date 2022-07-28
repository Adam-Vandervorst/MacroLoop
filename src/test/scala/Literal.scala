import munit.FunSuite
import macroloop.*


class LiteralFunSuite extends FunSuite:
  inline def assertCodeMatches[T](inline obtained: T, inline expected: T): Unit =
    assertEquals(show(translateCode(obtained, expected)), show(expected))

class LiteralIntRange extends LiteralFunSuite:
  test("forEach") {
    assertCodeMatches(IntRange.forEach(1, 4, 1)(println), {
      var i = 1
      while i < 4 do
        val x = i
        println(x)
        i += 1
    }: scala.Unit)
  }

  test("forEachUnrolled") {
    assertCodeMatches(IntRange.forEachUnrolled(1, 4, 1)(println), {
      println(1)
      println(2)
      println(3)
    }: scala.Unit)
  }

class LiteralIt extends LiteralFunSuite:
  test("forEach") {
    val listIt = List('a', 'b', 'c')

    assertCodeMatches(IterableIt.forEach(listIt)(println), {
      val it = listIt.iterator
      while it.hasNext do
        val v = it.next()
        println(v)
    }: Unit)
  }

  test("forEachCart2") {
    val listIt = List('a', 'b', 'c')
    val rangeIt = List.range(1, 10)

    assertCodeMatches(IterableIt.forEachCart2(rangeIt, listIt)((c, i) => println((c, i))), {
      val it1 = rangeIt.iterator
      while it1.hasNext do
        val v1 = it1.next()
        val it2 = listIt.iterator
        while it2.hasNext do
          val v2 = it2.next()
          println((v1, v2))
    }: Unit)
  }

class LiteralArrayIndex extends LiteralFunSuite:
  test("forEach") {
    val a = Array(1, 2, 3)
    assertCodeMatches(ArrayIndex.forEach(a)(println), {
      val size = a.length
      var i = 0
      while i < size do
        val x = a(i)
        println(x)
        i += 1
    }: Unit)
  }

  test("forEachUnrolledN") {
    val a = Array(1, 2, 3)
    assertCodeMatches(ArrayIndex.forEachUnrolledN(3)(a)(println), {
      val size = a.length
      val r = size % 3
      var i = 0
      while i < r do
        val x = a(i)
        println(x)
        i += 1
      while i < size do
        val x0 = a(i)
        println(x0)
        val x1 = a(i + 1)
        println(x1)
        val x2 = a(i + 2)
        println(x2)
        i += 3
    }: Unit)
  }

  test("forallException") {
    import macroloop.macros.Break
    val a = Array(1, 2, 3)
    assertCodeMatches(ArrayIndex.forallException(a)(_ % 2 == 1), {
      try
        val size = a.length
        var i = 0
        while i < size do
          if {val v = a(i); v % 2 == 1} then i += 1
          else throw Break
        true
      catch case Break => false
    }: Boolean)
  }

  test("forallCondition") {
    val a = Array(1, 2, 3)
    assertCodeMatches(ArrayIndex.forallCondition(a)(_ % 2 == 1), {
      val size = a.length
      var b = true
      var i = 0
      while b && i < size do
        if {val v = a(i); v % 2 == 1} then i += 1
        else b = false
      b
    }: Boolean)
  }

class LiteralConstantTuple extends LiteralFunSuite:
  test("forEachUnrolled TupleN") {
    assertCodeMatches(ConstantTuple.forEachUnrolled(ConstantList.toTuple22(ConstantTuple.constToList['a' *: 1 *: 2 *: EmptyTuple]))(println), {
      println('a')
      println(1)
      println(2)
    }: Unit)
  }

  test("forEachUnrolled Tuple22") {
    assertCodeMatches(ConstantTuple.forEachUnrolled(('a', 1, None))(println), {
      println('a')
      println(1)
      val v = None
      println(v)
    }: Unit)
  }


class LiteralArgsTuple extends LiteralFunSuite:
  test("forEachUnrolled") {
    assertCodeMatches(ConstantArgs.forEachUnrolled('a', 1, None)(println), {
      println('a')
      println(1)
      val v = None
      println(v)
    }: Unit)
  }
