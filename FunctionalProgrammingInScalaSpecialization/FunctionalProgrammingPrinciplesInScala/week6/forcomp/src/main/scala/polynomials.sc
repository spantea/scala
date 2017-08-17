object polynomials {

  class Poly(val terms0: Map[Int, Double]) {

    val terms = terms0 withDefaultValue 0.0

      def this(bindings: (Int, Double)*) = this(bindings.toMap)
//    def + (other: Poly): Poly = {
//
//      val terms1: Map[Int, Double] = for {
//        (exp, coeff) <- terms
//        otherCoeff = other.terms(exp)
//      } yield (exp, coeff + otherCoeff)
//
//
//      new Poly(other.terms ++ terms1)
//    }

//    def + (other: Poly): Poly = new Poly(terms ++ (other.terms map adjust))

    def + (other: Poly): Poly = new Poly((other.terms foldLeft terms)(addTerm))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exp, coeff) = term
      terms + (exp -> (coeff + terms(exp)))
    }

    def adjust(pair: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = pair
      (exp, terms(exp) + coeff)
    }

    override def toString: String = (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }

  val m1: Map[Int, Double] = Map(2 -> 2.0, 1 -> 8.0, 0 -> 16.0)
  val m2: Map[Int, Double] = Map(2 -> 1.5, 3 -> 12.0, 5 -> 11.0)
  val p1 = new Poly(m1)
  val p2 = new Poly(m2)

  p1 + p2



}