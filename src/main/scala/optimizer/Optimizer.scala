package yafl.optimizer

import yafl.syntax.{InfixOperator, Syntax, TermTree}
import yafl.typer.{Type, TypedProgram}

object Optimizer:

  /** Returns `program` optimized. */
  def optimize(program: TypedProgram): TypedProgram =
    val (optimized, updated) = constantFoldRecursively(program.syntax, program.types)
    TypedProgram(optimized, updated)

  /** Substitutes constant expressions in `tree` with their results, returning a an updated syntax
    * tree along with a map from each term to its type.
    */
  private def constantFoldRecursively(
      tree: Syntax[TermTree], types: TypedProgram.TypeAssignments
  ): (Syntax[TermTree], TypedProgram.TypeAssignments) = {
    constantFold(tree) match
      case Some(s) =>
        // Constant folding succeeded; return the updated tree.
        (s, Map(s -> types(tree)))

      case _ => tree.value match
        case e: TermTree.TermApplication =>
          // Apply the optimization recursively.
          val (f, ts) = constantFoldRecursively(e.abstraction, types)
          val (a, us) = constantFoldRecursively(e.argument, types)
          val updated = Syntax(TermTree.TermApplication(f, a), tree.span)

          // Fold the result if possible.
          constantFold(updated) match
            case Some(s) => (s, Map(s -> types(tree)))
            case _ => (updated, (ts ++ us).updated(updated, types(tree)))

        case _ =>
          (tree, Map(tree -> types(tree)))
  }

  /** Returns a literal denoting the result of `tree` iff it represents a constant expression. */
  private def constantFold(tree: Syntax[TermTree]): Option[Syntax[TermTree]] =
    import TermTree.TermApplication as F
    tree.value match
      case F(Syntax(F(InfixOperator(f), IntegerConstant(left)), _), IntegerConstant(right)) =>
        f match
          case InfixOperator.Add | InfixOperator.Sub | InfixOperator.Mul | InfixOperator.Div =>
            val n = f match
              case InfixOperator.Add => left + right
              case InfixOperator.Sub => left - right
              case InfixOperator.Mul => left * right
              case InfixOperator.Div => left / right
              case _ => 0
            val newTree = Syntax(TermTree.IntegerLiteral(n), tree.span)
            Some(newTree)
          case _ =>
            val b = f match
              case InfixOperator.Eq => left == right
              case InfixOperator.Neq => left != right
              case InfixOperator.Lt => left < right
              case InfixOperator.Gt => left > right
              case InfixOperator.Lte => left <= right
              case InfixOperator.Gte => left >= right
              case _ => false
            val newTree = Syntax(TermTree.BooleanLiteral(b), tree.span)
            Some(newTree)
      case _ => None

end Optimizer

/** A pattern for recognizing integer constants. */
private object IntegerConstant:

  def unapply(s: Syntax[TermTree]): Option[Int] =
    s match
      case Syntax(TermTree.IntegerLiteral(n), _) => Some(n)
      case _ => None

end IntegerConstant
