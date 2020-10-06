package controlstructures

import scala.io.Source

object ControlStructuresHomework {

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  sealed trait Result
  final case class FinalResult(comand: Command, result: Double) extends Result

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    import Command._
    val input = x.trim.toLowerCase().split(" ").toList;

    input match {
      case List("")                                           => Left(ErrorMessage("Empty"))
      case _ :: xs if xs.map(_.toDoubleOption).contains(None) => Left(ErrorMessage("Incorrect input"))
      case x :: xs =>
        x match {
          case "divide"                  => Right(Divide(xs.head.toDouble, xs.tail.head.toDouble))
          case "sum"                     => Right(Sum(xs.map(_.toDouble)))
          case "average"                 => Right(Average(xs.map(_.toDouble)))
          case "min"                     => Right(Min(xs.map(_.toDouble)))
          case "max"                     => Right(Max(xs.map(_.toDouble)))
          case _                         => Left(ErrorMessage("Incorrect input"))
        }
    }
  }

  def calculate(x: Command): Either[ErrorMessage, Result] = {
    import Command._

    x match {
      case Divide(_, 0)               => Left(ErrorMessage("can't devide with 0"))
      case Divide(stNumber, ndNumber) => Right(FinalResult(s"division of ${stNumber} and ${ndNumber} : ${stNumber / ndNumber}"))
      case Sum(numbers)               => Right(FinalResult(s"sum of ${numbers.mkString(" ")} : ${numbers.sum}"))
      case Min(numbers)               => Right(FinalResult(s"minimum of ${numbers.mkString(" ")} : ${numbers.min}"))
      case Max(numbers)               => Right(FinalResult(s"maximum  of ${numbers.mkString(" ")} : ${numbers.max}"))
      case Average(numbers)           => Right(FinalResult(s"average of ${numbers.mkString(" ")} : ${numbers.sum / numbers.length}"))
      case _                          => Left(ErrorMessage("something went wrong"))
    }
  }

  def renderResult(x: Result): String = {
    case FinalResult(resultToRender) => resultToRender
  }

   def process(x: String): String = {
    (
        for {
            command <- parseCommand(x)
            result <- calculate(command)
        } yield result
    ).fold(left => s"something went wrong: ${left.value}", right => renderResult(right))
  }

  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
