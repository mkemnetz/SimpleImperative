package luc

import scala.collection.mutable.ListBuffer
import StatementParser._
import SimpleImperative._

object mainSimple {

  var input = new ListBuffer[String]

  def Repl(s: String): Unit = s match {
    case "quit;" => println("exit."); System.exit(0)
    case "help;" => println("ouput usage text here")
    case "dump;" => GlobalStore.Watch
    case "clear;" => GlobalStore.Reset
    case _ => {
      if (s.endsWith(";")) {
        val line = s.replace(';', ' ').trim()
        if (line.length() > 0)
          input += (line + "\n")
        Execute()
        input.clear()
      } else {
        input += s
      }
    }
  }

  def Execute(): Boolean = {

    try {
      //todo: maybe only parse class here

      val arr: Array[Statement] = input.filter(s => s.trim() != "")
        .map(s => (
          if (StatementParser.parseAll(StatementParser.clazz, s) != null) {
            null
          } else StatementParser.parseAll(StatementParser.expr, s).get))
        .toArray

      if (arr.length == 0) {
        println("parse expression error!")
        return false
      }

      val parseStatement = if (arr.length == 1) arr.head else new Sequence(arr: _*)

      if (SimpleValidator.Check(parseStatement)) {

        if (parseStatement != null) {
          //only for debug
          println(parseStatement)

          GlobalStore.Allocation(parseStatement)

          //GlobalStore.Watch
          SimpleImperative.apply(GlobalStore.Memory)(parseStatement)
        }
        return true
      } else {
        System.err.println("Valid Check error!")
      }
    } catch {
      case e: Exception => println(e.getMessage());
    }
    false
  }

  def main(args: Array[String]) {
    GlobalStore.Watch
    Iterator.continually(Console.readLine).foreach(line => Repl(line.trim()))
  }

}