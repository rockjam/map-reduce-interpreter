package com.github.rockjam.mapreduce.ast

// expressions are evaluated to values. values could be numeric values or numeric sequences
sealed trait Expression extends Product with Serializable

case class Arithmetic(left: Expression, op: Operator, right: Expression)
    extends Expression

// identifier evaluates to it's value
case class Identifier(name: String) extends Expression

// numeric literal evaluates to itself
case class NumericLiteral(value: Long) extends Expression

// num seq evaluates to itself.
// there could be numeric ranges, when we know only start value and end value
// there could be numeric sequences, when we know each element of sequence
case class NumSeq(start: Expression, end: Expression) extends Expression

case class SeqNumbers(numbers: Seq[Expression]) extends Expression {
  override def toString: String = numbers.map(_.asInstanceOf[NumericLiteral].value).toString()
}

// lambda evaluates to numeric value
case class Lambda(param: Seq[Identifier], body: Expression) extends Expression

// map evaluates to numeric sequence
// seq is either NumSeq or Identifier
case class Map(seq: Expression, lambda: Lambda) extends Expression

// reduce evaluates to numeric value
case class Reduce(seq: Expression, zero: Expression, lambda: Lambda) extends Expression
