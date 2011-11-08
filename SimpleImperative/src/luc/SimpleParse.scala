package luc

import scala.util.parsing.combinator._

object StatementParser extends JavaTokenParsers {
  def expr: Parser[Statement] = (
    statement ~ "+" ~ expr ^^ { case l ~ _ ~ r => Plus(l, r) }
    | statement ~ "-" ~ expr ^^ { case l ~ _ ~ r => Minus(l, r) }
    | statement ~ "=" ~ expr ^^ { case l ~ _ ~ r => Assignment(l, r) }
    | statement ~ "*" ~ expr ^^ { case l ~ _ ~ r => Times(l, r) }
    | statement ~ "/" ~ expr ^^ { case l ~ _ ~ r => Div(l, r) }
    | statement)
  def statement: Parser[Statement] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
    | "var" ~ ident ^^ { case _ ~ e => new Variable(e) }
    | "while" ~ expr ~ expr ^^ { case _ ~ l ~ r => While(l, r) }
    | "new" ~ clazz ^^ { case _ ~ e => New(e) }
    | "(" ~> expr <~ ")" ^^ { case e => e }
    | "{" ~> repsep(expr, ",") <~ "}" ^^ { case ss => Sequence(ss: _*) }
    | ident ~ "." ~ ident ^^ { case r ~ _ ~ f => Selection(Variable(r), f) }
    | ident ^^ { case s => Variable(s) }
    )
  def clazz: Parser[Clazz] = (
   "struct" ~ ident ~ "{" ~ repsep(ident, ",") ~ "}" ^^ { case _ ~ e ~ _ ~ s ~ _ => Clazz(s: _*) })
//  def value: Parser[Any] = obj
//  def arr: Parser[Seq[String]] = "(" ~> repsep(stringLiteral, ",") <~ ")"
//  def member: Parser[Any] = stringLiteral | stringLiteral ~ "." ~ value
//  def obj: Parser[Any] = "{" ~ repsep(member, ",") ~ "}"

}

