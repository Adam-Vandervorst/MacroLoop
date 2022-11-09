package be.adamv.macroloop.collection

import be.adamv.macroloop.utils.Repeat
import scala.compiletime.ops.int.*
import scalanative.unsafe.Nat

import Nat.*

type NatToInt[N <: Nat] <: Int = N match
  case _0 => 0
  case _1 => 1
  case _2 => 2
  case _3 => 3
  case _4 => 4
  case _5 => 5
  case _6 => 6
  case _7 => 7
  case _8 => 8
  case _9 => 9
  case Digit2[n1, n0] => 10*NatToInt[n1] + NatToInt[n0]
  case Digit3[n2, n1, n0] => 100*NatToInt[n2] + NatToInt[Digit2[n1, n0]]
  case Digit4[n3, n2, n1, n0] => 1000*NatToInt[n3] + NatToInt[Digit3[n2, n1, n0]]
  case Digit5[n4, n3, n2, n1, n0] => 10000*NatToInt[n4] + NatToInt[Digit4[n3, n2, n1, n0]]
  case Digit6[n5, n4, n3, n2, n1, n0] => 100000*NatToInt[n5] + NatToInt[Digit5[n4, n3, n2, n1, n0]]
  case Digit7[n6, n5, n4, n3, n2, n1, n0] => 1000000*NatToInt[n6] + NatToInt[Digit6[n5, n4, n3, n2, n1, n0]]
  case Digit8[n7, n6, n5, n4, n3, n2, n1, n0] => 10000000*NatToInt[n7] + NatToInt[Digit7[n6, n5, n4, n3, n2, n1, n0]]
  case Digit9[n8, n7, n6, n5, n4, n3, n2, n1, n0] => 100000000*NatToInt[n8] + NatToInt[Digit8[n7, n6, n5, n4, n3, n2, n1, n0]]

type TupleToDigit[Tup <: Repeat[9, Nat]] <: Nat = Tup match
  case (0, 0, 0, 0, 0, 0, 0, 0, n0) => n0 & Nat
  case (0, 0, 0, 0, 0, 0, 0, n1, n0) => Digit2[n1, n0]
  case (0, 0, 0, 0, 0, 0, n2, n1, n0) => Digit3[n2, n1, n0]
  case (0, 0, 0, 0, 0, n3, n2, n1, n0) => Digit4[n3, n2, n1, n0]
  case (0, 0, 0, 0, n4, n3, n2, n1, n0) => Digit5[n4, n3, n2, n1, n0]
  case (0, 0, 0, n5, n4, n3, n2, n1, n0) => Digit6[n5, n4, n3, n2, n1, n0]
  case (0, 0, n6, n5, n4, n3, n2, n1, n0) => Digit7[n6, n5, n4, n3, n2, n1, n0]
  case (0, n7, n6, n5, n4, n3, n2, n1, n0) => Digit8[n7, n6, n5, n4, n3, n2, n1, n0]
  case (n8, n7, n6, n5, n4, n3, n2, n1, n0) => Digit9[n8, n7, n6, n5, n4, n3, n2, n1, n0]

type IntToNat[N <: Int] <: Nat = N match
  case 0 => _0
  case 1 => _1
  case 2 => _2
  case 3 => _3
  case 4 => _4
  case 5 => _5
  case 6 => _6
  case 7 => _7
  case 8 => _8
  case 9 => _9
  case N => TupleToDigit[(
      IntToNat[N/100000000],
      IntToNat[N/10000000],
      IntToNat[N/1000000],
      IntToNat[N/100000],
      IntToNat[N/10000],
      IntToNat[N/1000],
      IntToNat[N/100],
      IntToNat[N/10],
      IntToNat[N])]

