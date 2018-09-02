package com.github.rockjam.mapreduce.interpreter

import com.github.rockjam.mapreduce.ast._

import scala.collection.immutable

object Interpreter {

  def apply(statements: Seq[Statement]): Unit = {
    def evaluate(statement: Statement)(env: DynamicEnvironment): DynamicEnvironment  = statement match {
      case Print(s) =>
        print(s)
        env
      case Out(expr) =>
        val evaluated = evalExpression(expr)(env)
        println(evaluated)
        env
      case Assignment(identifier, expression) =>
        val evaluated = evalExpression(expression)(env)
        env + (identifier -> evaluated)
    }

    val emptyEnv: DynamicEnvironment = immutable.Map.empty
    statements.foldLeft(emptyEnv) { (acc, el) => evaluate(el)(acc) }
  }

  def evalExpression(expr: Expression)(env: DynamicEnvironment): Expression = expr match {
    case Arithmetic(l, op, r) =>
      val left = evalExpression(l)(env).asInstanceOf[NumericLiteral]
      val right = evalExpression(r)(env).asInstanceOf[NumericLiteral]

      op match {
        case Operator.Add => NumericLiteral(left.value + right.value)
        case Operator.Sub => NumericLiteral(left.value - right.value)
        case Operator.Div => NumericLiteral(left.value / right.value)
        case Operator.Mul => NumericLiteral(left.value * right.value)
        case Operator.Exp => NumericLiteral(left.value ^ right.value)
      }
    case id: Identifier => env(id)
    case Lambda(_, body) =>
      evalExpression(body)(env)
    case Map(seq, lambda) =>
      val seqElements: Seq[Expression] = getSeqElements(evalExpression(seq)(env))

      SeqNumbers(
        seqElements.map { el => evalExpression(lambda)(env = immutable.Map(lambda.param.head -> el)) }
      )
    case Reduce(seq, zero, lambda) =>
      val seqElements: Seq[Expression] = getSeqElements(evalExpression(seq)(env))

      val zeroElement = evalExpression(zero)(env).asInstanceOf[NumericLiteral]

      seqElements.foldLeft[Expression](zeroElement) { (acc, el) =>
        val Seq(param1, param2) = lambda.param
        evalExpression(lambda)(env = immutable.Map(param1 -> acc, param2 -> el))
      }
    case NumSeq(start, end) =>
      NumSeq(evalExpression(start)(env), evalExpression(end)(env))
    case num: NumericLiteral => num
  }

  private def getSeqElements(expr: Expression): Seq[Expression] = expr match {
    case NumSeq(start, end) =>
      (start.asInstanceOf[NumericLiteral].value to end.asInstanceOf[NumericLiteral].value).map(NumericLiteral)
    case SeqNumbers(numbers) => numbers
  }

}
