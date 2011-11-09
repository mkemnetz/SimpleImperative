package luc

object SimpleValidator {
  def Check(s: Statement): Boolean = s match {
    case While(guard, body) => guard match {
      case Constant(c) => Check(body)
      case Variable(v) => Check(body)
      case Assignment(left, right) => Check(guard) && Check(body)
      case Selection(receiver, field) => Check(body)
      case Sequence(statements @ _*) => Check(statements.last) && Check(body)
      case _ => false
    }
    case Assignment(left, right) => left match {
      case Variable(v) => true
      case Sequence(statements @ _*) => statements.map(s => Check(s)).foldLeft(true)(_ && _)
      case Assignment(left, right) => Left(left).isRight
      case _ => false
    }
    case Sequence(statements @ _*) => statements.map(s => Check(s)).foldLeft(true)(_ && _)
    case Plus(left, right) => Check(left) && Check(right)
    case Minus(left, right) => Check(left) && Check(right)
    case Times(left, right) => Check(left) && Check(right)
    case Div(left, right) => Check(left) && Check(right)
    case Selection(r, f) => Check(r)
    case _ => false
  }
}