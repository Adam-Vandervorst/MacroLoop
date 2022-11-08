/*
import munit.FunSuite

import be.adamv.macroloop.collection.*
import be.adamv.macroloop.collection.algorithm.*

object TestHasCycle:
  val pos1 = Matrix(
    ('B', 'C', 'B'),
    ('C', 'A', 'A'),
    ('B', 'A', 'A')
  )

  val pos2 = Matrix(
    ('A', 'A', 'B'),
    ('A', 'A', 'B'),
    ('B', 'A', 'A')
  )

  val pos3 = Matrix(
    ('B', 'A', 'A', 'A'),
    ('B', 'A', 'X', 'A'),
    ('B', 'A', 'A', 'A')
  )

  val pos4 = Matrix(
    ('X', 'X', 'X', 'O'),
    ('X', 'I', 'X', 'X'),
    ('X', 'I', 'I', 'X'),
    ('X', 'X', 'X', 'X'),
  )

  val neg1 = Matrix(
    ('B', 'C', 'B'),
    ('C', 'C', 'A'),
    ('B', 'A', 'A')
  )

  val neg2 = Matrix.from[1, 1, Char](Iterator.single('X'))

  val neg3 = Matrix(
    ('X', 'X', 'X', 'O'),
    ('X', 'Q', 'P', 'X'),
    ('X', 'Q', 'Q', 'X'),
    ('X', 'X', 'X', 'X'),
  )


class Algorithm extends FunSuite:
  test("pos hasCycle") {
    import TestHasCycle.*
    assert(hasCycle(pos1))
    assert(hasCycle(pos2))
    assert(hasCycle(pos3))
    assert(hasCycle(pos4))
  }

  test("neg hasCycle") {
    import TestHasCycle.*
    assert(!hasCycle(neg1))
    assert(!hasCycle(neg2))
    assert(!hasCycle(neg3))
  }
*/
