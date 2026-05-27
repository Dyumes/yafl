
import yafl.SourceFile
import yafl.optimizer.Optimizer
import yafl.parser.Parser
import yafl.syntax.{Syntax, TermTree}
import yafl.typer.{TypedProgram, Typer}

final class OptimizerTests extends munit.FunSuite:

  test("constant folding"):
    val optimized = optimize("1 + 2 + 3")
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(6) => ()

  test("constant folding 2"):
    val optimized = optimize("1 * 2 / 2")
    (optimized.syntax.value: @unchecked) match
      case TermTree.IntegerLiteral(1) => ()

  test("constant folding 3"):
    val optimized = optimize("1 > 2")
    (optimized.syntax.value: @unchecked) match
      case TermTree.BooleanLiteral(false) => ()

  test("constant folding 4"):
    val optimized = optimize("1 < 2")
    (optimized.syntax.value: @unchecked) match
      case TermTree.BooleanLiteral(true) => ()

  test("constant folding 5"):
    val optimized = optimize("1 == 1")
    (optimized.syntax.value: @unchecked) match
      case TermTree.BooleanLiteral(true) => ()

  test("constant folding 6"):
    val optimized = optimize("1 != 2")
    (optimized.syntax.value: @unchecked) match
      case TermTree.BooleanLiteral(true) => ()

  test("constant folding 7"):
    val optimized = optimize("1 <= 2")
    (optimized.syntax.value: @unchecked) match
      case TermTree.BooleanLiteral(true) => ()

  test("constant folding 8"):
    val optimized = optimize("1 >= 2")
    (optimized.syntax.value: @unchecked) match
      case TermTree.BooleanLiteral(false) => ()

  test("normalization"):
    import TermTree.TermApplication as F
    val optimized = optimize("x + 1")
    (optimized.syntax.value : @unchecked) match
      case F(lhs, Syntax(TermTree.IntegerLiteral(1), _)) =>
        (lhs.value : @unchecked) match
          case F(_, Syntax(TermTree.Variable("x"), _)) => ()

  /** Compiles `input` to a WebAssembly module and returns an instance of it. */
  private def optimize(input: String): TypedProgram =
    Optimizer.optimize(Typer.check(Parser.parse(SourceFile("test", input))))

end OptimizerTests
