trait Expression {
    def map[T](λ: Expression => T): T = λ(this)
}

case class Val(value: Int) extends Expression
case class Var(name: String) extends Expression
case class NegOp(ghs: Expression) extends Expression
case class AddOp(lhs: Expression, rhs: Expression) extends Expression
case class SubOp(lhs: Expression, rhs: Expression) extends Expression
case class MulOp(lhs: Expression, rhs: Expression) extends Expression
case class DivOp(lhs: Expression, rhs: Expression) extends Expression

object Entrance {
    def evaluate(tree: Expression): Double = tree match {
        case Val(value)         => value.asInstanceOf[Double]
        case Var(_)             => throw new Exception("unable to evaluate variable")
        case NegOp(ghs)         => -evaluate(ghs)
        case AddOp(lhs, rhs)    => evaluate(lhs) + evaluate(rhs)
        case SubOp(lhs, rhs)    => evaluate(lhs) - evaluate(rhs)
        case MulOp(lhs, rhs)    => evaluate(lhs) * evaluate(rhs)
        case DivOp(lhs, rhs)    => evaluate(lhs) / evaluate(rhs)
    }


    def substitute(
        tree: Expression,
        subs: Map[String, Int]
    ): Double = {
        def remap(subtree: Expression): Expression =
            subtree.map {
                // Traverse the tree, searching for variables to replace
                case x: Val => x
                case x@Var(name) =>
                    subs.get(name) match {
                        case Some(value)    => Val(value)
                        case None           => x
                    }
                case NegOp(x) => NegOp(x.map(remap))
                case AddOp(l, r) => AddOp(l.map(remap), r.map(remap))
                case SubOp(l, r) => SubOp(l.map(remap), r.map(remap))
                case MulOp(l, r) => MulOp(l.map(remap), r.map(remap))
                case DivOp(l, r) => DivOp(l.map(remap), r.map(remap))
            }

        evaluate(remap(tree))
    }


    def simplify(tree: Expression): Expression = tree match {
        // Can't simplify terminals
        case x: Val => x
        case x: Var => x

        case NegOp(ghs) =>
            simplify(ghs) match {
                // Double negative
                case NegOp(x)   => x

                // No simplification possible
                case x          => NegOp(x)
            }

        case AddOp(lhs, rhs) =>
            (simplify(lhs), simplify(rhs)) match {
                // Adding 0
                case (Val(0), r)        => r
                case (l, Val(0))        => l

                // Addition of negative
                case (l, NegOp(ghs))    => SubOp(l, ghs)

                // Factor
                case (MulOp(a, x), MulOp(b, y))
                              if x == y => MulOp(AddOp(a, b), x)
                case (MulOp(a, x), MulOp(y, b))
                              if x == y => MulOp(AddOp(a, b), x)
                case (MulOp(x, a), MulOp(b, y))
                              if x == y => MulOp(AddOp(a, b), x)
                case (MulOp(x, a), MulOp(y, b))
                              if x == y => MulOp(AddOp(a, b), x)

                // Group like terms
                case (l, r) if l == r   => MulOp(Val(2), l)

                // No simplification possible
                case (l, r)             => AddOp(l, r)

        }

        case SubOp(lhs, rhs) =>
            (simplify(lhs), simplify(rhs)) match {
                // Subtract 0
                case (l, Val(0))        => l

                // Unary minus
                case (Val(0), r)        => NegOp(r)

                // Subtract oneself
                case (x, y) if x == y   => Val(0)
            }

        case MulOp(lhs, rhs) =>
            (simplify(lhs), simplify(rhs)) match {
                // Times 0
                case (Val(0), _) => Val(0)
                case (_, Val(0)) => Val(0)

                // Times 1
                case (Val(1), x) => x
                case (x, Val(1)) => x

                // No simplification possible
                case (l, r)      => MulOp(l, r)
            }

        case DivOp(lhs, rhs)    =>
            (simplify(lhs), simplify(rhs)) match {
                // Division by 1
                case (x, Val(1))                => x

                // Cancelation
                case (MulOp(x, g), y) if x == y => g
                case (MulOp(g, x), y) if x == y => g

                // No simplification possible
                case (l, r)                     => DivOp(l, r)
            }
    }

    def main(args: Array[String]) = {
        println(evaluate(
            MulOp(NegOp(AddOp(Val(5), Val(6))), Var("x"))
        ))
    }
}

