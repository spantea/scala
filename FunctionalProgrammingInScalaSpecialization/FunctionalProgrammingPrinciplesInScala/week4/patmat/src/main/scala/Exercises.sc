abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}
object Zero extends Nat {
  def isZero = true
  def predecessor = throw new NoSuchElementException()
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = if (that.isZero) Zero else throw new NoSuchElementException()
}
class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor: Nat = n
  def + (that: Nat): Nat = new Succ(n + that)
  def - (that: Nat): Nat = if (that.isZero) this else n - that.predecessor
}
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}
class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}
class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object List {
  //List(x1,x2)
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))

  //List(x1)
  def apply[T](x: T): List[T] = new Cons(x, new Nil)

  //List()
  def apply() = new Nil
}

trait Expr {
  def eval: Int = this match {
    case Num(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }

  def show: String = this match {
      case Var(x) => x.toString
      case Num(n) => n.toString
      case Sum(e1, e2) => e1.show + " + " + e2.show
      case Prod(e1, e2) => (e1, e2) match {
        case (Num(n), Num(m)) => n.toString + " * " + m.toString
        case (Num(n), Var(x)) => n.toString + " * " + x.toString
        case (Var(x), Num(n)) => x.toString + " * " + n.toString
        case (Var(x), Sum(a, b)) => x.toString + " * " + "(" + e2.show + ")"
        case (Sum(x, y), Num(n)) => "(" + e1.show + ")" + " * " + e2.show
        case (Sum(x, y), Var(n)) => "(" + e1.show + ")" + " * " + e2.show
        case (Sum(x, y), Sum(a, b)) => "(" + e1.show + ")" + " * " + "(" + e2.show + ")"
        case (Sum(_, _), Prod(_, _)) => "(" + e1.show + ")" + " * " + e2.show
        case (Prod(_, _), Sum(_, _)) => e1.show + " * " + "(" + e2.show + ")"
        case (Prod(_, _), Prod(_, _)) => e1.show + " * " + e2.show
        // ... to be continued
      }
  }
}
case class Var(x: String) extends Expr
case class Num(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

val expr = new Sum(new Num(1), new Num(2))
expr.eval

val s1 = Sum(Prod(Num(2), Var("x")), Var("y"))
s1.show

val s2 = Prod(Sum(Num(1), Var("x")), Sum(Num(1), Var("y")))
s2.show

val s3 = Prod(Var("y"), Sum(Num(1), Num(2)))
s3.show

val s4 = Prod(Sum(Num(1), Num(2)), Var("x"))
s4.show

val s5 = Prod(Prod(Var("x"), Num(3)), Sum(Var("k"), Num(5)))
s5.show

val s6 = Prod(Sum(Var("x"), Num(9)), Prod(Var("x"), Num(4)))
s6.show