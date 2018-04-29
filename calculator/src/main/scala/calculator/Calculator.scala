package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
	def computeValues(
			namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {

					val result = namedExpressions.map{ case (e,k) =>{

						(e, Signal(eval(k(),namedExpressions)))}
					}
					result
	}

	def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match{
	case Plus(a:Expr,b:Expr) => eval(a,references) + eval(b,references)
	case Minus(a:Expr,b:Expr) => eval(a,references) - eval(b,references)
	case Times(a:Expr,b:Expr) => eval(a,references) * eval(b,references)
	case Divide(a:Expr,b:Expr) =>if(eval(b,references)!=0) eval(a,references) / eval(b,references) else Double.NaN
	case a:Literal => {

		a.v
	}
	case r:Ref => if(references.contains(r.name) &&(!contain(references(r.name)(),r))) eval(getReferenceExpr(r.name,references),references) else Double.NaN
	case _ => {

		Double.NaN
	}
	}
	//check if a given axpression contains a ref
	def contain(expr:Expr,ref:Ref) : Boolean = expr match{
	case b:Ref => (b.name == ref.name)
	case Minus(a:Expr,b:Expr) => contain(a,ref) && contain(b,ref)
	case Times(a:Expr,b:Expr) => contain(a,ref) && contain(b,ref)
	case Divide(a:Expr,b:Expr) =>contain(a,ref) && contain(b,ref)
	case Plus(a:Expr,b:Expr) =>contain(a,ref) && contain(b,ref)
	case _ => false
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
