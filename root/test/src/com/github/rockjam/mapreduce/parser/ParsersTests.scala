package com.github.rockjam.mapreduce.parser

import com.github.rockjam.mapreduce.ast._
import fastparse.core.Parsed
import utest._

object ParsersTests extends TestSuite {

  def tests: Tests = Tests {

    "print parser" - {
      "parse valid input" - {
        val Parsed.Success(string, _) =
          Parsers.print.parse("""print "hello world"""")

        assert(string == Print("hello world"))
      }
      "parse input with whitespace on the end" - {
        val Parsed.Success(string, _) =
          Parsers.print.parse("""print "hello world" """)

        assert(string == Print("hello world"))
      }
// FIXME
//      "fail when something comes after string literal" - {
//        assert(
//          parseFailed(Parsers.print.parse("""print "hello world"ashdjks"""))
//        )
//      }
      "fail on unclosed string" - {
        assert(
          parseFailed(Parsers.print.parse("""print "hello"""))
        )
      }
      "fail when there is no string" - {
        assert(
          parseFailed(Parsers.print.parse("""print """))
        )
      }
      "fail when there is identifier after print" - {
        assert(
          parseFailed(Parsers.print.parse("""print x"""))
        )
      }
    }

    "arithmetic expression parser" - {
      "parse valid input" - {
        val Parsed.Success(output, _) =
          Parsers.arithmeticExpression.parse("i + 4")

        assert(
          output == Arithmetic(Identifier("i"), Operator.Add, NumericLiteral(4))
        )
      }
      "parse simple expression" - {
        val Parsed.Success(output, _) =
          Parsers.arithmeticExpression.parse("6 * 8")

        assert(
          output == Arithmetic(NumericLiteral(6),
                               Operator.Mul,
                               NumericLiteral(8))
        )
      }
// FIXME
//      "parse expression with 3 parts" - {
//        val Parsed.Success(output, _) =
//          Parsers.arithmeticExpression.parse("6 * 8 + i")
//
//        assert(
//          output == Arithmetic(
//            Arithmetic(NumericLiteral(6), Operator.Mul, NumericLiteral(8)),
//            Operator.Add,
//            Identifier("i")
//          )
//        )
//      }
    }

    "assignment parser" - {
      "parse numeric literal assignment" - {
        val Parsed.Success(assignment, _) =
          Parsers.assignment.parse("val x = 22")

        assert(
          assignment == Assignment(Identifier("x"), NumericLiteral(22))
        )
      }
      "parse input with different spaces" - {
        val inputs = Seq(
          "val x=22",
          "val x =22",
          "val x= 22",
          "val x = 22 "
        )

        inputs.foreach { input =>
          val Parsed.Success(assignment, _) =
            Parsers.assignment.parse(input)

          assert(
            assignment == Assignment(Identifier("x"), NumericLiteral(22))
          )
        }
      }
      "parse arithmetic expression assignment" - {
        val Parsed.Success(assignment, _) =
          Parsers.assignment.parse("val x = i + 5")

        assert(
          assignment == Assignment(Identifier("x"),
                                   Arithmetic(Identifier("i"),
                                              Operator.Add,
                                              NumericLiteral(5)))
        )
      }
      "parse numeric sequence assignment" - {
        val Parsed.Success(assignment, _) =
          Parsers.assignment.parse("val x = {1,5}")

        assert(
          assignment == Assignment(
            Identifier("x"),
            NumSeq(
              NumericLiteral(1),
              NumericLiteral(5)
            )
          )
        )
      }
      "parse numeric sequence assignment" - {
        val Parsed.Success(assignment, _) =
          Parsers.assignment.parse("val x = {1,n}")

        assert(
          assignment == Assignment(
            Identifier("x"),
            NumSeq(
              NumericLiteral(1),
              Identifier("n")
            )
          )
        )
      }
      "fail on input without assignment" - {
        assert(
          parseFailed(Parsers.assignment.parse("val")),
          parseFailed(Parsers.assignment.parse("val ")),
          parseFailed(Parsers.assignment.parse("val x")),
          parseFailed(Parsers.assignment.parse("val x =")),
          parseFailed(Parsers.assignment.parse("val x = "))
        )
      }
    }

    "sequence parser" - {
      "parse valid input" - {
        val Parsed.Success(numSeq, _) = Parsers.sequence.parse("{10, 34}")

        assert(numSeq == NumSeq(NumericLiteral(10), NumericLiteral(34)))
      }
      "parse input without whitespaces" - {
        val Parsed.Success(numSeq, _) = Parsers.sequence.parse("{1,2}")

        assert(numSeq == NumSeq(NumericLiteral(1L), NumericLiteral(2L)))
      }
      "parse input with spaces between numbers" - {
        val inputs = Seq("{1, 2}", "{1 , 2}", "{1 ,2}")

        inputs.foreach { input =>
          val Parsed.Success(numSeq, _) = Parsers.sequence.parse(input)
          assert(numSeq == NumSeq(NumericLiteral(1L), NumericLiteral(2L)))
        }
      }
      "parse input with numbers padded with spaces" - {
        val inputs = Seq("{ 1,2 }", "{ 1, 2 }", "{ 1 , 2 }", "{ 1 ,2 }")

        inputs.foreach { input =>
          val Parsed.Success(numSeq, _) = Parsers.sequence.parse(input)
          assert(numSeq == NumSeq(NumericLiteral(1L), NumericLiteral(2L)))
        }
      }
      "fail on unclosed }" - {
        assert(
          parseFailed(Parsers.sequence.parse("{1,2"))
        )
      }
      "fail when there is no ," - {
        assert(
          parseFailed(Parsers.sequence.parse("{1 2}"))
        )
      }
      "fail on incomplete sequence" - {
        assert(
          parseFailed(Parsers.sequence.parse("{1"))
        )
      }
    }

    "lambda parser" - {
      "lambda1" - {
        "parse valid input" - {
          val Parsed.Success(lambda, _) = Parsers.lambda1.parse("i -> i")

          assert(
            lambda == Lambda(
              Seq(Identifier("i")),
              Identifier("i")
            )
          )
        }
        "parse simple expression" - {
          val Parsed.Success(lambda, _) = Parsers.lambda1.parse("i -> i * 2")

          assert(
            lambda == Lambda(
              Seq(Identifier("i")),
              Arithmetic(
                Identifier("i"),
                Operator.Mul,
                NumericLiteral(2)
              )
            )
          )
        }
      }
      "lambda2" - {
        "parse valid input" - {
          val Parsed.Success(lambda, _) = Parsers.lambda1.parse("x y -> x + y")

          assert(
            lambda == Lambda(
              Seq(Identifier("x"), Identifier("y")),
              Arithmetic(
                Identifier("x"),
                Operator.Add,
                Identifier("y")
              )
            )
          )
        }
      }
    }

    "map parser" - {
      "parse valid input" - {
        val Parsed.Success(map, _) = Parsers.map.parse("map({1,3}, i -> i * 2)")

        assert(
          map == Map(
            seq = NumSeq(NumericLiteral(1), NumericLiteral(3)),
            lambda = Lambda(
              Seq(Identifier("i")),
              Arithmetic(
                Identifier("i"),
                Operator.Mul,
                NumericLiteral(2)
              )
            )
          )
        )
      }
      "parse input where sequence is identifier" - {
        val Parsed.Success(map, _) = Parsers.map.parse("map(seq, i -> i * 2)")

        assert(
          map == Map(
            seq = Identifier("seq"),
            lambda = Lambda(
              Seq(Identifier("i")),
              Arithmetic(
                Identifier("i"),
                Operator.Mul,
                NumericLiteral(2)
              )
            )
          )
        )
      }

    }

    "reduce parser" - {
      "parse valid input" - {
        val Parsed.Success(reduce, _) = Parsers.reduce.parse("reduce({0,10}, 0, x y -> x + y)")

        assert(
          reduce == Reduce(
            NumSeq(NumericLiteral(0), NumericLiteral(10)),
            NumericLiteral(0),
            Lambda(
              Seq(Identifier("x"), Identifier("y")),
              Arithmetic(
                Identifier("x"),
                Operator.Add,
                Identifier("y")
              )
            ))
        )

      }
    }

    "program parser" - {
      "parse multi-line input" - {
        val program =
          """val x = 5
            |val y = map({1,3}, i -> i * 2)
            |print "hello world"
            |out x + y
          """.stripMargin

        val Parsed.Success(expressions, _) = Parsers.program.parse(program)
        val expectedExpressions = Seq(
          Assignment(Identifier("x"), NumericLiteral(5)),
          Assignment(
            Identifier("y"),
            Map(
              NumSeq(NumericLiteral(1),NumericLiteral(3)),
              Lambda(
                Seq(Identifier("i")),
                Arithmetic(
                  Identifier("i"),
                  Operator.Mul,
                  NumericLiteral(2)
                )
              )
            )
          ),
          Print("hello world"),
          Out(Arithmetic(Identifier("x"), Operator.Add,Identifier("y")))
        )
        assert(
          expressions == expectedExpressions
        )
      }
    }
  }

  def parseFailed(result: Parsed[_, _, _]): Boolean = {
    result.isInstanceOf[Parsed.Failure[_, _]]
  }

}
