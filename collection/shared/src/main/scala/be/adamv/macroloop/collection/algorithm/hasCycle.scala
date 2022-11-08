/*
package be.adamv.macroloop.collection.algorithm

import be.adamv.macroloop.IntRange
import be.adamv.macroloop.collection.Matrix

inline def hasCycle[M <: Int & Singleton, N <: Int & Singleton, A](m: Matrix[M, N, A]): Boolean =
  val visited = Matrix.fill[M, N, Boolean](false)

  val directionX: Array[Int] = Array(-1, 0, 1, 0)
  val directionY: Array[Int] = Array(0, 1, 0, -1)

  def isCycle(x: Int, y: Int, px: Int = -1, py: Int = -1): Boolean =
    visited(x, y) = true

    IntRange.existsUnrolled(0, 4, 1) { k =>
      val nx = x + directionX(k)
      val ny = y + directionY(k)
      m.containsPosition(nx, ny) &&
      m(nx, ny) == m(x, y) &&
      !(px == nx && py == ny) && (
        visited(nx, ny) ||
        isCycle(nx, ny, x, y)
      )
    }
  end isCycle

  visited.existsItem((i, j, visitedij) => !visitedij && isCycle(i, j))
end hasCycle
*/
