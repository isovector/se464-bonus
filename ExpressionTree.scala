trait Expr {
    def map(λ: Expr => Expr) = λ(this)
}

case class Neg(ghs: Expr) extends Expr {
    // Unary operator should recurse map down its argument
    override def map(λ: Expr => Expr): Expr =
        λ(Neg(ghs.map(λ)))
}

abstract class BinaryExpr extends Expr {
    val lhs: Expr
    val rhs: Expr

    // Binary operators should recurse map down both sides
    override def map(λ: Expr => Expr): Expr = {
        val args = (lhs.map(λ), rhs.map(λ))
        (this match {
            case _: Add => Add
            case _: Sub => Sub
            case _: Mul => Mul
            case _: Div => Div
            case _      => throw new Exception("danger! danger!")
        }).tupled(args)
    }
}

case class Val(value: Int) extends Expr
case class Var(name: String) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends BinaryExpr
case class Sub(lhs: Expr, rhs: Expr) extends BinaryExpr
case class Mul(lhs: Expr, rhs: Expr) extends BinaryExpr
case class Div(lhs: Expr, rhs: Expr) extends BinaryExpr

object Entrance {
    def evaluate(tree: Expr): Double = tree match {
        case Val(value)         => value.asInstanceOf[Double]
        case Var(_)             => throw new Exception("unable to evaluate variable")
        case Neg(ghs)         => -evaluate(ghs)
        case Add(lhs, rhs)    => evaluate(lhs) + evaluate(rhs)
        case Sub(lhs, rhs)    => evaluate(lhs) - evaluate(rhs)
        case Mul(lhs, rhs)    => evaluate(lhs) * evaluate(rhs)
        case Div(lhs, rhs)    => evaluate(lhs) / evaluate(rhs)
    }


    def substitute(tree: Expr, subs: Map[String, Int]): Double =
        evaluate(tree.map { branch =>
            branch match {
                // Replace variables with their subs, if we have one
                case x@Var(name) => subs.get(name) match {
                    case Some(value) => Val(value)
                    case _           => x
                }

                // Do nothing otherwise
                case x           => x
            }
        })


    def simplify(tree: Expr): Expr = tree match {
        // Can't simplify terminals
        case x: Val => x
        case x: Var => x

        case Neg(ghs) =>
            simplify(ghs) match {
                // Double negative
                case Neg(x)   => x

                // No simplification possible
                case x          => Neg(x)
            }

        case Add(lhs, rhs) =>
            (simplify(lhs), simplify(rhs)) match {
                // Adding 0
                case (Val(0), r)        => r
                case (l, Val(0))        => l

                // Addition of negative
                case (l, Neg(ghs))    => Sub(l, ghs)

                // Factor
                case (Mul(a, x), Mul(b, y))
                              if x == y => Mul(Add(a, b), x)
                case (Mul(a, x), Mul(y, b))
                              if x == y => Mul(Add(a, b), x)
                case (Mul(x, a), Mul(b, y))
                              if x == y => Mul(Add(a, b), x)
                case (Mul(x, a), Mul(y, b))
                              if x == y => Mul(Add(a, b), x)

                // Group like terms
                case (l, r) if l == r   => Mul(Val(2), l)

                // No simplification possible
                case (l, r)             => Add(l, r)
        }

        case Sub(lhs, rhs) =>
            (simplify(lhs), simplify(rhs)) match {
                // Subtract 0
                case (l, Val(0))        => l

                // Unary minus
                case (Val(0), r)        => Neg(r)

                // Subtract oneself
                case (x, y) if x == y   => Val(0)
            }

        case Mul(lhs, rhs) =>
            (simplify(lhs), simplify(rhs)) match {
                // Times 0
                case (Val(0), _) => Val(0)
                case (_, Val(0)) => Val(0)

                // Times 1
                case (Val(1), x) => x
                case (x, Val(1)) => x

                // No simplification possible
                case (l, r)      => Mul(l, r)
            }

        case Div(lhs, rhs)    =>
            (simplify(lhs), simplify(rhs)) match {
                // Division by 1
                case (x, Val(1))                => x

                // Cancelation
                case (Mul(x, g), y) if x == y => g
                case (Mul(g, x), y) if x == y => g

                // No simplification possible
                case (l, r)                     => Div(l, r)
            }
    }

    def main(args: Array[String]) = {
        println(evaluate(
            Mul(Neg(Add(Val(5), Val(6))), Var("x"))
        ))
    }
}

