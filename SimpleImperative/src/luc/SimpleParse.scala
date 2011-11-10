package luc

import scala.util.parsing.combinator._

object StatementParser extends JavaTokenParsers {
  def expr: Parser[Statement] = (
    statement ~ "+" ~ statement ^^ { case l ~ _ ~ r => Plus(l, r) }
    | statement ~ "-" ~ statement ^^ { case l ~ _ ~ r => Minus(l, r) }
    | statement ~ "*" ~ statement ^^ { case l ~ _ ~ r => Times(l, r) }
    | statement ~ "/" ~ statement ^^ { case l ~ _ ~ r => Div(l, r) }
    | statement ~ "=" ~ expr ^^ { case l ~ _ ~ r => Assignment(l, r) }
    | statement)
  def statement: Parser[Statement] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
    | "var" ~ ident ^^ { case _ ~ e => new Variable(e) }
    | "while" ~ expr ~ expr ^^ { case _ ~ l ~ r => While(l, r) }
   // | "new" ~ ident ^^ { case _ ~ s => New(GlobalStore.GetClass(s)) }
    | "new" ~ clazz ^^ { case _ ~ e => New(e) }
    | "(" ~> expr <~ ")" ^^ { case e => e }
    | "{" ~> repsep(expr, ",") <~ "}" ^^ { case ss => Sequence(ss: _*) }
    | ident ~ "." ~ ident ^^ { case r ~ _ ~ f => Selection(Variable(r), f) }
    | ident ^^ { case s => Variable(s) })
  def clazz: Parser[Clazz] = (
    "struct" ~ ident ~ "{" ~ repsep(ident, ",") ~ "}" ^^ {
      case _ ~ e ~ _ ~ s ~ _ => {
    	  //insert to reference table 
          //
          GlobalStore.PutClass(e, Clazz(s: _*))
      }}
    | ident ^^  { case s => GlobalStore.GetClass(s) }
    )
  //  def value: Parser[Any] = obj
  //  def arr: Parser[Seq[String]] = "(" ~> repsep(stringLiteral, ",") <~ ")"
  //  def member: Parser[Any] = stringLiteral | stringLiteral ~ "." ~ value
  //  def obj: Parser[Any] = "{" ~ repsep(member, ",") ~ "}"

}

