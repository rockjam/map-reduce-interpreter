package com.github.rockjam.mapreduce

import com.github.rockjam.mapreduce.ast.NumSeq
import com.github.rockjam.mapreduce.parser.Parsers

object Main {

  def main(args: Array[String]): Unit = {
    val input = args(0)

    val result = Parsers.sequence
      .parse(input)
      .fold(
        onFailure = (a, b, c) => s"Failed to parse ${input}",
        onSuccess = {
          case (NumSeq(start, end), _) =>
            s"Parsed: start: ${start}, end: ${end}"
        }
      )

    println(result)
  }

}
