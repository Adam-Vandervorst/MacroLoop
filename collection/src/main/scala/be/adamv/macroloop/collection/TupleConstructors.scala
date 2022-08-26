package be.adamv.macroloop.collection

object TupleConstructors:
  import be.adamv.macroloop.collection.macros.*

  transparent inline def vectorApply[Tup <: Tuple](inline elements: Tup) =
    ${ concreteVectorImpl[
      Tuple.Size[Tup],
      Tuple.Union[Tup]
    ]('elements) }

  type AsTuple[X] <: Tuple = X match
    case Tuple => X & Tuple
  type Flatten[Tup <: Tuple] = 
    Tuple.FlatMap[Tup, [X <: Tuple.Union[Tup]] =>> AsTuple[X]]
  
  transparent inline def matrixApply[Tup <: NonEmptyTuple](inline elements: Tup) =
    ${ concreteMatrixImpl[
      Tuple.Size[Tup],
      Tuple.Size[AsTuple[Tuple.Head[Tup]]],
      Tuple.Union[Flatten[Tup]]
    ]('elements) }
