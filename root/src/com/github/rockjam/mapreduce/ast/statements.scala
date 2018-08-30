package com.github.rockjam.mapreduce.ast

sealed trait Statement extends Product with Serializable

case class Assignment(identifier: Identifier, expression: Expression)
    extends Statement

case class Out(expression: Expression) extends Statement

case class Print(s: String) extends Statement
