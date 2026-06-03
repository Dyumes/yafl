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
    /*
    constantFold(tree) match
      case Some(s) =>
        // Constant folding succeeded; return the updated tree.
        (s, Map(s -> types(tree)))
*/
      //case _ =>
      val (child, ts0) =  tree.value match
        case e: TermTree.TermApplication =>
          val (f, ts) = constantFoldRecursively(e.abstraction, types)
          val (a, us) = constantFoldRecursively(e.argument, ts)
          (Syntax(TermTree.TermApplication(f, a), tree.span), us)

        case e : TermTree.Conditional =>
          val (c, ts1) = constantFoldRecursively(e.condition,types)
          val (s, ts2) = constantFoldRecursively(e.success, ts1)
          val (f, ts3) = constantFoldRecursively(e.failure, ts2)
          (Syntax(TermTree.Conditional(c,s,f), tree.span), ts3)

        case e : TermTree.Binding =>
          val (init, ts1) = constantFoldRecursively(e.initializer, types)
          val (body, ts2) = constantFoldRecursively(e.body, ts1)
          (Syntax(TermTree.Binding(e.name, init, body), tree.span), ts2)

        case e : TermTree.TermAbstraction =>
          val (body, ts) = constantFoldRecursively(e.body, types)
          (Syntax(TermTree.TermAbstraction(e.parameter, e.ascription, body), tree.span), ts)

        case e : TermTree.TypeAbstraction =>
          val (body, ts) = constantFoldRecursively(e.body, types)
          (Syntax(TermTree.TypeAbstraction(e.parameter, body), tree.span), ts)

        case e : TermTree.TypeApplication =>
          val (a, ts) = constantFoldRecursively(e.abstraction, types)
          (Syntax(TermTree.TypeApplication(a, e.argument), tree.span), ts)

        case e: TermTree.RecursiveAbstraction =>
          val (body, ts) = constantFoldRecursively(e.definition, types)
          (Syntax(TermTree.RecursiveAbstraction(e.name, e.ascription, body), tree.span), ts)

        case _ =>
          (tree, types)

      val ts = if child != tree then
        ts0.updated(child, types(tree))
      else
        ts0

      optimizerTasks(child, ts, ts0)
  }

  private def optimizerTasks(tree: Syntax[TermTree], types: TypedProgram.TypeAssignments, ts0 : TypedProgram.TypeAssignments): (Syntax[TermTree], TypedProgram.TypeAssignments) = {
    normalize(tree, types) match
      case Some((norm, tsNorm)) =>
        return constantFoldRecursively(norm, tsNorm)
      case None =>

    constantFold(tree, types) match
      case Some((folded, tsFold)) =>
        return constantFoldRecursively(folded, tsFold)
      case None =>
        (tree, ts0)

    deadCodeElimination(tree, types) match
      case Some((elim, tsElim)) =>
        return constantFoldRecursively(elim, tsElim)
      case None =>
        (tree, types)
  }

  private def normalize(tree: Syntax[TermTree], types: TypedProgram.TypeAssignments):Option[(Syntax[TermTree], TypedProgram.TypeAssignments)] = {
    val type0 = types(tree)

    tree.value match
      case TermTree.TermApplication(f, Syntax(TermTree.Binding(name, init, body),_ )) =>
        val app = Syntax(TermTree.TermApplication(f, body), tree.span)
        val treee = Syntax(TermTree.Binding(name, init, app), tree.span)
        val ts = types.updated(app, type0).updated(treee, type0)
        Some((treee, ts))
      case TermTree.TermApplication(Syntax(TermTree.Binding(name, init, body), _ ), a) =>
        val app = Syntax(TermTree.TermApplication(body, a), tree.span)
        val treee = Syntax(TermTree.Binding(name, init, app), tree.span)
        val ts = types.updated(app, type0).updated(treee, type0)
        Some((treee, ts))
      case TermTree.TermApplication(app @ Syntax(TermTree.TermApplication(ope @ InfixOperator(f), x), _ ), IntegerConstant(c))  if f == InfixOperator.Add && !x.value.isInstanceOf[TermTree.IntegerLiteral] =>
        val const = Syntax(TermTree.IntegerLiteral(c), tree.span)
        val in = Syntax(TermTree.TermApplication(ope, const), app.span)
        val treee = Syntax(TermTree.TermApplication(in, x), tree.span)
        val ts = types.updated(const, Type.Ground.Int).updated(in, types(app)).updated(treee, type0)
        Some((treee, ts))
      case TermTree.TermApplication(appOut @ Syntax(TermTree.TermApplication(op1 @ InfixOperator(f1), Syntax(TermTree.TermApplication(appInner @ Syntax(TermTree.TermApplication(op2 @ InfixOperator(f2), IntegerConstant(c1)), _), x), _)), _), IntegerConstant(c2))  if f1 == InfixOperator.Add && f2 == InfixOperator.Add =>
          val const = Syntax(TermTree.IntegerLiteral(c1 + c2), tree.span)
          val in = Syntax(TermTree.TermApplication(op1, const), appInner.span)
          val treee = Syntax(TermTree.TermApplication(in, x), tree.span)
          val ts = types.updated(const, Type.Ground.Int).updated(in, types(appOut)).updated(treee, type0)
          Some((treee, ts))
      case _ =>
        None
  }

  private def deadCodeElimination(tree: Syntax[TermTree], types: TypedProgram.TypeAssignments):Option[(Syntax[TermTree], TypedProgram.TypeAssignments)] = {
    val type0 = types(tree)

    tree.value match
      case TermTree.Conditional(Syntax(TermTree.BooleanLiteral(true), _), success, _) =>
        Some((success, types.updated(success, type0)))
      case TermTree.Conditional(Syntax(TermTree.BooleanLiteral(false), _), _, failure) =>
        Some((failure, types.updated(failure, type0)))
      case _ => None

  }

  /** Returns a literal denoting the result of `tree` iff it represents a constant expression. */
  private def constantFold(tree: Syntax[TermTree], types: TypedProgram.TypeAssignments): Option[(Syntax[TermTree], TypedProgram.TypeAssignments)] =
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
            val ts = types.updated(newTree, Type.Ground.Int)
            Some(newTree, ts)
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
            val ts = types.updated(newTree, Type.Ground.Bool)
            Some(newTree, ts)
      case _ => None
end Optimizer

/** A pattern for recognizing integer constants. */
private object IntegerConstant:

  def unapply(s: Syntax[TermTree]): Option[Int] =
    s match
      case Syntax(TermTree.IntegerLiteral(n), _) => Some(n)
      case _ => None

end IntegerConstant
