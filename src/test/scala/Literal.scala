import munit.FunSuite

import macroloop.*

class Literal extends FunSuite:
  test("IntRange forEach") {
    assertEquals(show(IntRange.forEach(1, 4, 1)(println)), """({
  var i: scala.Int = 1
  while (i.<(4)) {
    val x: scala.Int = i
    scala.Predef.println(x)
    i = i.+(1)
  }
}: scala.Unit)""")
  }

  test("IntRange forEachUnrolled") {
    assertEquals(show(IntRange.forEachUnrolled(1, 4, 1)(println)), """({
  scala.Predef.println(1)
  scala.Predef.println(2)
  scala.Predef.println(3)
}: scala.Unit)""")
  }

  test("ArrayIndex forEach") {
    val a = Array(1, 2, 3)
    assertEquals(show(ArrayIndex.forEach(a)(println)), """({
  val size: scala.Int = a.length
  var i: scala.Int = 0
  while (i.<(size)) {
    val x: scala.Int = a.apply(i)
    scala.Predef.println(x)
    i = i.+(1)
  }
}: scala.Unit)""")
  }

  test("ArrayIndex forEachUnrolledN") {
    val a = Array(1, 2, 3)
    assertEquals(show(ArrayIndex.forEachUnrolledN(3)(a)(println)), """({
  val size: scala.Int = a.length
  val r: scala.Int = size.%(3)
  var i: scala.Int = 0
  while (i.<(r)) {
    val x$proxy1: scala.Int = a.apply(i)
    scala.Predef.println(x$proxy1)
    i = i.+(1)
  }
  while (i.<(size)) {
    val x$proxy2: scala.Int = a.apply(i.+(0))
    scala.Predef.println(x$proxy2)
    val x$proxy3: scala.Int = a.apply(i.+(1))
    scala.Predef.println(x$proxy3)
    val x$proxy4: scala.Int = a.apply(i.+(2))
    scala.Predef.println(x$proxy4)
    i = i.+(3)
  }
}: scala.Unit)""")
  }

  test("ArrayIndex forallException") {
    val a = Array(1, 2, 3)
    assertEquals(show(ArrayIndex.forallException(a)(_ % 2 == 1)), """(try {
  val size: scala.Int = a.length
  var i: scala.Int = 0
  while (i.<(size)) if ({
    val _$1: scala.Int = a.apply(i)
    _$1.%(2).==(1)
  }) i = i.+(1) else throw macroloop.macros.Break
  true
} catch {
  case macroloop.macros.Break =>
    false
}: scala.Boolean)""")
  }

  test("ArrayIndex forallCondition") {
    val a = Array(1, 2, 3)
    assertEquals(show(ArrayIndex.forallCondition(a)(_ % 2 == 1)), """({
  val size: scala.Int = a.length
  var b: scala.Boolean = true
  var i: scala.Int = 0
  while (b.&&(i.<(size))) if ({
    val _$2: scala.Int = a.apply(i)
    _$2.%(2).==(1)
  }) i = i.+(1) else b = false

  (b: scala.Boolean)
}: scala.Boolean)""")
  }
  
  test("ConstantTuple forEachUnrolled".ignore) {
//    transparent inline def t1 = 'a' *: 1 *: 0 *: EmptyTuple
//    transparent inline def t2: ('a', 1, 0) = ('a', 1, 0)

//    ConstantTuple.forEachUnrolled('a' *: 1 *: 0 *: EmptyTuple)(println)
  }

  
  
  test("ConstantArgs forEachUnrolled") {
    assertEquals(show(ConstantArgs.forEachUnrolled('a', 1, None)(println)), """({
  scala.Predef.println('a')
  scala.Predef.println(1)
  val x: scala.None.type = scala.None
  scala.Predef.println(x)
}: scala.Unit)""")
  }
