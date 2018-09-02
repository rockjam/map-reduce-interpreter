package com.github.rockjam.mapreduce

import java.nio.file.{Files, Paths}

import com.github.rockjam.mapreduce.interpreter.Interpreter
import com.github.rockjam.mapreduce.parser.Parsers

object Main {

  def main(args: Array[String]): Unit = {
    val file = args(0)

    val content = new String(Files.readAllBytes(Paths.get(file)))

    println("=== the program is ===")
    println(content)
    println("======================")

    Parsers.program
      .parse(content)
      .fold(
        onFailure = (a, b, c) =>
          println(s"Failed to parse ${file}"),
        onSuccess = {
          case (statements, _) =>
            println("=== statements are ===")
            println(statements.mkString("\n"))
            println("======================")

            Interpreter(statements)
        }
      )
  }

}
