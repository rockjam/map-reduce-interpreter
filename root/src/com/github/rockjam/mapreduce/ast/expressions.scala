package com.github.rockjam.mapreduce.ast

sealed trait Expression

case class Arithmetic(left: Expression, op: Operator, right: Expression)
    extends Expression

sealed trait Value extends Expression with Product with Serializable

case class Identifier(name: String) extends Value

case class NumericLiteral(value: Long) extends Value

case class NumSeq(start: Value, end: Value) extends Expression

case class Lambda(param: Identifier, body: Expression) extends Expression

case class Map(seq: NumSeq, lambda: Lambda) extends Expression

case class Reduce(seq: NumSeq, zero: Value, lambda: Lambda) extends Expression
