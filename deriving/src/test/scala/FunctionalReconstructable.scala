import munit.FunSuite
import be.adamv.macroloop.deriving.Reconstructable
import be.adamv.macroloop.show


case class Pair[A](a1: A, a2: A) derives Reconstructable
case class Interleaved[A](v1: Int, a1: A, v2: Int, a2: A, v3: Int) derives Reconstructable
case class Constants[A](v1: Int, v2: String) derives Reconstructable

class FunctionalReconstructable extends FunSuite {
  test("reconstructFromTuple product") {
    assert(Pair(1, 2).reconstructFromTuple(('a', 'b')) == Pair('a', 'b'))
    assert(Interleaved(10, 1, 20, 2, 30).reconstructFromTuple(('a', 'b')) == Interleaved(10,'a',20,'b',30))
    assert(Constants(10, "XXX").reconstructFromTuple(EmptyTuple) == Constants(10, "XXX"))
  }
}
