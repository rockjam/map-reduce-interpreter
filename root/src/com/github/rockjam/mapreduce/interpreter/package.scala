package com.github.rockjam.mapreduce

import com.github.rockjam.mapreduce.ast.{Expression, Identifier}

package object interpreter {
  type DynamicEnvironment = Map[Identifier, Expression]
}
