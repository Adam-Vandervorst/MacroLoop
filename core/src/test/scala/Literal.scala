import munit.FunSuite

import be.adamv.macroloop.*

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
    }: Unit)
  }

  test("forEachUnrolled") {
    assertCodeMatches(IntRange.forEachUnrolled(1, 4, 1)(println), {
      println(1)
      println(2)
      println(3)
    }: Unit)
  }

  test("forall") {
    assertCodeMatches(IntRange.forall(0, 10, 2)(_ < 100), {
      var i: Int = 0
      while i < 10 do
        if i < 100 then i += 2
        else i = 2147483647
      i < 2147483647
    }: Boolean)
  }

  test("exists") {
    assertCodeMatches(IntRange.exists(0, 10, 2)(_ < 100), {
      var i: Int = 0
      while i < 10 do
        if i < 100 then i = 2147483647
        else i += 2
      i == 2147483647
    }: Boolean)
  }

  test("forallUnrolled") {
    val m = 2
    assertCodeMatches(IntRange.forallUnrolled(0, 10, 2)(_ % m == 0), {
      0 % m == 0 && 2 % m == 0 && 4 % m == 0 && 6 % m == 0 && 8 % m == 0
    }: Boolean)
  }

  test("existsUnrolled") {
    val m = 2
    assertCodeMatches(IntRange.existsUnrolled(0, 10, 2)(_ % m == 0), {
      0 % m == 0 || 2 % m == 0 || 4 % m == 0 || 6 % m == 0 || 8 % m == 0
    }: Boolean)
  }


  test("forEachZipped2") {
    assertCodeMatches(IntRange.forEachZipped2((0, 10, 1), (10, 101, 10))((x, y) => println(x - y)), {
      var i1 = 0
      var i2 = 10
      while i1 < 10 && i2 < 101 do
        println(i1 - i2)
        i1 += 1
        i2 += 10
    }: Unit)
  }

  test("forEachZipped2 abbreviated") {
    assertCodeMatches(IntRange.forEachZipped2((0, 10), (10, 20))((x, y) => println(x - y)), {
      var i1 = 0
      var i2 = 10
      while i1 < 10 && i2 < 20 do
        println(i1 - i2)
        i1 += 1
        i2 += 1
    }: Unit)
  }

  test("forEachZipped") {
    assertCodeMatches(IntRange.forEachZipped[3]((0, 10, 1), (10, 101, 10), (1, 6, 2))((t: (Int, Int, Int)) => println(t._1 - t._2)), {
      var i3 = 1
      var i2 = 10
      var i1 = 0
      while i1 < 10 && i2 < 101 && i3 < 6 do
        println(i1 - i2)
        i1 += 1
        i2 += 10
        i3 += 2
    }: Unit)
  }

  test("forEachZipped match") {
    val n = 10
    val l = 2 + n * 2
    val a = new Array[Int](l)
    assertCodeMatches(IntRange.forEachZipped[2]((2, l, 2), (0, Int.MaxValue, 3)) { case (x: Int, y: Int) =>
      a(x) = y
      a(x + 1) = y
    }, {
      var i2: Int = 0
      var i1: Int = 2
      while i1 < l && i2 < 2147483647 do
        a(i1) = i2
        a(i1 + 1) = i2
        i1 += 2
        i2 += 3
    }: Unit)
  }


class LiteralIt extends LiteralFunSuite:
  test("forEach") {
    val list = List('a', 'b', 'c')

    assertCodeMatches(IterableIt.forEach(list)(println), {
      val it = list.iterator
      while it.hasNext do
        val v = it.next()
        println(v)
    }: Unit)
  }

  test("forEachCart2") {
    val list = List('a', 'b', 'c')
    val range = List.range(1, 10)

    assertCodeMatches(IterableIt.forEachCart2(range, list)((c, i) => println((c, i))), {
      val it1 = range.iterator
      while it1.hasNext do
        val v1 = it1.next()
        val it2 = list.iterator
        while it2.hasNext do
          val v2 = it2.next()
          println((v1, v2))
    }: Unit)
  }

  test("forEachCart") {
    val list = List('a', 'b', 'c')
    val range = List.range(1, 10)
    val array = Array(1, 2, 3)

    assertCodeMatches(IterableIt.forEachCart[(Char, Int, Int)]((list, array, range))(println), {
      val xit: Iterator[Char] = list.iterator
      while (xit.hasNext) {
        val x: Char = xit.next()
        val yit: Iterator[Int] = wrapIntArray(array).iterator
        while (yit.hasNext) {
          val y: Int = yit.next()
          val zit: Iterator[Int] = range.iterator
          while (zit.hasNext) {
            val z: Int = zit.next()
            val v = (x, y, z)
            println(v)
          }
        }
      }
    }: Unit)
  }

  test("forallExceptionCart match") {
    import be.adamv.macroloop.macros.Break
    val list = List('a', 'b', 'c')
    val range = List.range(1, 10)
    val array = Array(1, 2, 3)

    assertCodeMatches(IterableIt.forallExceptionCart[(Char, Int, Int)]((list, array, range)){ case (_, y, z) => y*z <= 10 }, {
      try
        val xit: Iterator[Char] = list.iterator
        while xit.hasNext do
          val x: Char = xit.next()
          val yit: Iterator[Int] = wrapIntArray(array).iterator
          while yit.hasNext do
            val y: Int = yit.next()
            val zit: Iterator[Int] = range.iterator
            while zit.hasNext do
              val z: Int = zit.next()
              if !(y*z <= 10) then throw Break
        true
      catch
        case Break => false
    }: Boolean)
  }

class LiteralSizedArrayIndex extends LiteralFunSuite:
  test("ofSize") {

    case class A(i: Int, j: Int, xs: List[Int])

//    println(show(SizedArrayIndex.ofSize[Int](10)))
//    println(show(SizedArrayIndex.ofSize[String](10)))
    println(show(SizedArrayIndex.ofSize[A](10)))
  }


  test("map") {
    val a = Array(1, 2, 3)

    assertCodeMatches(SizedArrayIndex.map(a, 3)(2 * _), {
      val na: Array[Int] = new Array[Int](3)
      var i: Int = 0
      while i < 3 do
        na(i) = 2*a(i)
        i += 1
      na
    }: Array[Int])
  }

  test("mapUnrolled") {
    val a = Array(1, 2, 3)

    assertCodeMatches(SizedArrayIndex.mapUnrolled(a, 3)(_ + 1), {
      val na: Array[Int] = new Array[Int](3)
      na(0) = a(0) + 1
      na(1) = a(1) + 1
      na(2) = a(2) + 1
      na
    }: Array[Int])
  }

  test("mapUnrolledN") {
    val a = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    assertCodeMatches(SizedArrayIndex.mapUnrolledN(3)(a, 10)(2*_), {
      val na: Array[Int] = new Array[Int](10)
      na(0) = 2*a(0)
      var i: Int = 1
      while i < 10 do
        na(i) = 2*a(i)
        i += 1
        na(i) = 2*a(i)
        i += 1
        na(i) = 2*a(i)
        i += 1
      na
    }: Array[Int])
  }

  test("flatMapFullyUnrolled") {
    val a = Array(1, 2, 3)

    assertCodeMatches(SizedArrayIndex.flatMapFullyUnrolled(a, 3)(x => Array(x, -x), 2), {
      val na: Array[Int] = new Array[Int](6)
      val ra1: Array[Int] = Array.apply(a.apply(0), -a.apply(0))
      na.update(0, ra1.apply(0))
      na.update(1, ra1.apply(1))
      val ra2: Array[Int] = Array.apply(a.apply(1), -a.apply(1))
      na.update(2, ra2.apply(0))
      na.update(3, ra2.apply(1))
      val ra3: Array[Int] = Array.apply(a.apply(2), -a.apply(2))
      na.update(4, ra3.apply(0))
      na.update(5, ra3.apply(1))
      na
    }: Array[Int])
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

  test("map") {
    val a = Array(1, 2, 3)

    assertCodeMatches(ArrayIndex.map(a)(2*_), {
      val size: Int = a.length
      val na: Array[Int] = new Array[Int](size)
      var i: Int = 0
      while i < size do
        na(i) = 2*a(i)
        i += 1
      na: Array[Int]
    }: Array[Int])
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
    import be.adamv.macroloop.macros.Break
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
  test("insert prepend append") {
    assertCodeMatches(ConstantTuple.insert(1, 2, 4)(2, 3), Tuple4[1, 2, 3, 4](1, 2, 3, 4))
    assertCodeMatches(ConstantTuple.prepend(1, 2, 3)(0), Tuple4[0, 1, 2, 3](0, 1, 2, 3))
    assertCodeMatches(ConstantTuple.append(1, 2, 3)(4), Tuple4[1, 2, 3, 4](1, 2, 3, 4))
  }

  test("concat") {
    assertCodeMatches(ConstantTuple.concat((1, 2, 3), (4, 5)), Tuple5[1, 2, 3, 4, 5](1, 2, 3, 4, 5))
  }

  test("forEachUnrolled TupleN") {
    assertCodeMatches(ConstantTuple.forEachUnrolled(ConstantList.toTuple22(ConstantTuple.constToList['a' *: 1 *: 2 *: EmptyTuple]))(println), {
      println('a')
      println(1)
      println(2)
    }: Unit)
  }

  test("compute forEachUnrolled Tuple22 linear") {
    val it = Iterator.iterate(0)(_ + 1)
    def compute = it.next()

    assertCodeMatches(ConstantTuple.forEachUnrolled((compute, 1, None))(println), {
      println(compute)
      println(1)
      println(None)
    }: Unit)
  }

  test("compute forEachUnrolled Tuple22") {
    val it = Iterator.iterate(0)(_ + 1)
    def compute = it.next()

    // needs any annotations because it isn't handled as a parametrized or poly function?
    assertCodeMatches(ConstantTuple.forEachUnrolled((compute, 1, None))(x => println((x, x))), {
      val c: Int = compute
      println(Tuple2[Any, Any](c, c))
      println(Tuple2[Any, Any](1, 1))
      println(Tuple2[Any, Any](None, None))
    }: Unit)
  }

  test("forEachBoundedUnrolled asInstanceOf") {
    assertCodeMatches(ConstantTuple.forEachBoundedUnrolled((1, 2, 3).asInstanceOf)((x: Int) => println(java.lang.Integer.toBinaryString(x))), {
      println(java.lang.Integer.toBinaryString(1))
      println(java.lang.Integer.toBinaryString(2))
      println(java.lang.Integer.toBinaryString(3))
    }: Unit)
  }

  test("tabulateUnrolled") {
    assertCodeMatches(ConstantTuple.tabulateUnrolled(4)(_*2),
      Tuple4.apply[0, 2, 4, 6](0, 2, 4, 6))
  }

  test("fillUnrolled") {
    val it = Iterator.iterate(0)(_ + 1)

    assertCodeMatches(ConstantTuple.fillUnrolled(4)(it.next()),
      Tuple4.apply[Int, Int, Int, Int](it.next(), it.next(), it.next(), it.next()))
  }

class LiteralArgsTuple extends LiteralFunSuite:
  test("forEachUnrolled") {
    assertCodeMatches(ConstantArgs.forEachUnrolled('a', 1, None)(println), {
      println('a')
      println(1)
      println(None)
    }: Unit)
  }
