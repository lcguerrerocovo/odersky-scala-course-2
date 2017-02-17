package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    val map1 = namedExpressions.filter(x => checkIfCyclic(x._1,namedExpressions))
    val map2 = namedExpressions.filterNot(x => checkIfCyclic(x._1,namedExpressions))
    map1.map(x => x._1 -> Signal(Double.NaN)) ++ map2.map(x => x._1 -> Signal(eval(x._2(),namedExpressions)))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
    case Literal(v) => v
    case Plus(a,b) => eval(a, references) + eval(b, references)
    case Minus(a,b) => eval(a, references) - eval(b, references)
    case Times(a,b) => eval(a, references) * eval(b, references)
    case Divide(a,b) => eval(a, references) / eval(b, references)
    case Ref(str) => eval(getReferenceExpr(str,references),references)
  }

  def checkIfCyclic(name: String, references: Map[String, Signal[Expr]]): Boolean = {
    def checkIfCyclic(expr: Expr): Boolean = expr match {
      case Plus(a, b) => checkIfCyclic(a) || checkIfCyclic(b)
      case Minus(a, b) => checkIfCyclic(a) || checkIfCyclic(b)
      case Times(a, b) => checkIfCyclic(a) || checkIfCyclic(b)
      case Divide(a, b) => checkIfCyclic(a) || checkIfCyclic(b)
      case Ref(str) => if (name == str) true else checkIfCyclic(getReferenceExpr(str, references))
      case _ => false
    }
    checkIfCyclic(getReferenceExpr(name,references))
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
