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
    for {
      (name, _) <- namedExpressions
      valueSignal = Signal(eval(namedExpressions(name)(), namedExpressions))
    } yield (name, valueSignal)
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {

    def loop(expr: Expr, refs: Set[Ref]): Double = expr match {
      case Literal(v) => v
      case Ref(name) => {
        if (refs.contains(Ref(name))) Double.NaN
        else loop(getReferenceExpr(name, references), refs + Ref(name))
      }
      case Plus(a, b) => loop(a, refs) + loop(b, refs)
      case Minus(a, b) => loop(a, refs) - loop(b, refs)
      case Times(a, b) => loop(a, refs) * loop(b, refs)
      case Divide(a, b) => loop(a, refs) / loop(b, refs)
    }

    loop(expr, Set())
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]): Expr = {

    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
