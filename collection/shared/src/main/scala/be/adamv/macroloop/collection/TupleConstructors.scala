/**
 * Contains fancy syntax constructors.
 * Separated out to avoid cyclic imports.
 */
package be.adamv.macroloop.collection

// TODO maybe this can be encapsulated better; how does that combine with the `export`?
//object TupleConstructors:
//  import be.adamv.macroloop.collection.macros.*
//
//  /**
//   * Allows for Vector(1, 2, 3) syntax, converting the tuple into the regular flat array.
//   * Deduces the correct size information.
//   */
//  transparent inline def vectorApply[Tup <: Tuple](inline elements: Tup) =
//    ${ concreteVectorImpl[
//      Tuple.Size[Tup],
//      Tuple.Union[Tup]
//    ]('elements) }
//
//  // TODO maybe the type errors can be improved by adding a type bound or doing this calculation in the macro.
//  type AsTuple[X] <: Tuple = X match
//    case Tuple => X & Tuple
//  type Flatten[Tup <: Tuple] =
//    Tuple.FlatMap[Tup, [X <: Tuple.Union[Tup]] =>> AsTuple[X]]
//
//  /**
//   * Allows for Matrix((1, 2), (3, 4)) syntax, converting the nested tuples into the regular flat array.
//   * Deduces the correct size information.
//   */
//  transparent inline def matrixApply[Tup <: NonEmptyTuple](inline elements: Tup) =
//    ${ concreteMatrixImpl[
//      Tuple.Size[Tup],
//      Tuple.Size[AsTuple[Tuple.Head[Tup]]],
//      Tuple.Union[Flatten[Tup]]
//    ]('elements) }
