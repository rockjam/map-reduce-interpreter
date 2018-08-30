package com.github.rockjam.mapreduce.ast

sealed trait Operator extends Product with Serializable

object Operator {
  case object Add extends Operator
  case object Sub extends Operator
  case object Div extends Operator
  case object Mul extends Operator
  case object Exp extends Operator
}
