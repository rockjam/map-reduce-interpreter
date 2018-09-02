package com.github.rockjam.mapreduce.parser

import com.github.rockjam.mapreduce.ast._
import fastparse.all._

object Parsers {

  /**
    * expr ::= expr op expr | (expr) | identifier | { expr, expr } | number | map(expr, identifier -> expr) | reduce(expr, expr, identifier identifier -> expr)
    * op ::= + | - | * | / | ^
    * stmt ::= val identifier = expr | out expr | print "string"
    * program ::= stmt | program stmt
    *
    * Example:
    * val n = 500
    * val sequence = map({0, n}, i -> (-1)^i / (2.0 * i + 1))
    * val pi = 4 * reduce(sequence, 0, x y -> x + y)
    * print "pi = "
    * out pi
    **/
  def parse(program: String): Either[Throwable, Seq[Statement]] = ???

  val program: P[Seq[Statement]] = P((statement ~ newLine).rep)

  val ws = P(" ".rep(1))
  val wsMaybe = P(" ".rep)
  val newLine = P("\n")

  val expression: P[Expression] = P(map | reduce | arithmeticExpression | identifier | numLiteral | sequence)

  val statement: P[Statement] = P(assignment | out | print)

  val identifier: P[Identifier] =
    P(AlfaNumeric.alfa ~ AlfaNumeric.alfaNum.rep).!.map(Identifier)
  val numLiteral: P[NumericLiteral] =
    P(AlfaNumeric.num.rep(min = 1)).!.map(s => NumericLiteral(s.toLong))

  val assignment =
    P("val" ~/ ws ~ identifier ~ wsMaybe ~ "=" ~ wsMaybe ~ expression ~ wsMaybe)
      .map {
        case (identifier, expression) => Assignment(identifier, expression)
      }

  val out = P("out" ~ ws ~ expression ~ wsMaybe).map { expr =>
    Out(expr)
  }

  val print =
    P("print" ~ ws ~ "\"" ~ CharsWhile(_ != '"').rep(1).! ~ "\"" ~ wsMaybe)
      .map { string =>
        Print(string)
      }

  val operator =
    P("+").map(_ => Operator.Add) |
      P("-").map(_ => Operator.Sub) |
      P("*").map(_ => Operator.Mul) |
      P("/").map(_ => Operator.Div) |
      P("^").map(_ => Operator.Exp)

  // TODO: allow arithmetic expression be the part of expression
  val arithmeticExpression: P[Arithmetic] = {
    val exp = identifier | numLiteral
    P(exp ~ wsMaybe ~ operator ~ wsMaybe ~ exp)
      .map { case (left, op, right) => Arithmetic(left, op, right) }
  }

  // TODO: I guess we shouldn't restrict number of parameters in parsing stage,
  // cause when it's wrong number of arguments - syntax is correct, semantics are not.
  def lambda(params: Int): P[Lambda] = P((identifier ~ wsMaybe).rep(min = params) ~ wsMaybe ~ "->" ~ wsMaybe ~ expression)
    .map { case (params, body) => Lambda(params, body) }

  val lambda1:P[Lambda] = lambda(1)

  val lambda2:P[Lambda] = lambda(2)

  val sequence: P[NumSeq] = {
    val seqElement = P(numLiteral | identifier)
    P("{" ~ wsMaybe ~ seqElement ~ wsMaybe ~ "," ~ wsMaybe ~ seqElement ~ wsMaybe ~ "}")
      .map { case (start, end) => NumSeq(start, end) } // ~wsMaybe ~ End
  }

  val map: P[Map] = P(
    "map" ~ "(" ~ wsMaybe ~ (sequence | identifier) ~ wsMaybe ~ "," ~ wsMaybe ~ lambda1 ~ wsMaybe ~ ")"
  ).map { case (seq, lambda) => Map(seq, lambda) }

  val reduce: P[Reduce] = P(
    "reduce" ~ "(" ~ wsMaybe ~ (sequence | identifier) ~ wsMaybe ~ "," ~ wsMaybe ~ (numLiteral | identifier) ~ wsMaybe ~ "," ~ wsMaybe ~ lambda2 ~ wsMaybe ~ ")"
  ).map { case (seq, zero, lambda) => Reduce(seq, zero, lambda)}

}

object AlfaNumeric {
  val alfaLower = CharIn('a' to 'z')
  val alfaUpper = CharIn('A' to 'Z')
  val num = CharIn('0' to '9')

  val alfa = alfaLower | alfaUpper

  val alfaNum = alfa | num
}
