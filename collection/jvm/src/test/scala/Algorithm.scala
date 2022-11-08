import munit.FunSuite

import be.adamv.macroloop.collection.*

object TestHasCycle:
  val pos1 = MatrixMNArray(
    ('B', 'C', 'B'),
    ('C', 'A', 'A'),
    ('B', 'A', 'A')
  )

  val pos2 = MatrixMNArray(
    ('A', 'A', 'B'),
    ('A', 'A', 'B'),
    ('B', 'A', 'A')
  )

  val pos3 = MatrixMNArray(
    ('B', 'A', 'A', 'A'),
    ('B', 'A', 'X', 'A'),
    ('B', 'A', 'A', 'A')
  )

  val pos4 = MatrixMNArray(
    ('X', 'X', 'X', 'O'),
    ('X', 'I', 'X', 'X'),
    ('X', 'I', 'I', 'X'),
    ('X', 'X', 'X', 'X'),
  )

  val neg1 = MatrixMNArray(
    ('B', 'C', 'B'),
    ('C', 'C', 'A'),
    ('B', 'A', 'A')
  )

  val neg2 = MatrixMNArray.asSingleElement('X')

  val neg3 = MatrixMNArray(
    ('X', 'X', 'X', 'O'),
    ('X', 'Q', 'P', 'X'),
    ('X', 'Q', 'Q', 'X'),
    ('X', 'X', 'X', 'X'),
  )


class Algorithm extends FunSuite:
  test("pos hasCycle") {
    import TestHasCycle.*
    assert(pos1.hasCycle)
    assert(pos2.hasCycle)
    assert(pos3.hasCycle)
    assert(pos4.hasCycle)
  }

  test("neg hasCycle") {
    import TestHasCycle.*
    assert(!neg1.hasCycle)
    assert(!neg2.hasCycle)
    assert(!neg3.hasCycle)
  }
