
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
    val optimized = optimize("(x: Int) => x + 1")
    (optimized.syntax.value: @unchecked) match
      case TermTree.TermAbstraction(_, _, Syntax(F(lhs, Syntax(TermTree.Variable("x"), _)), _)) =>
        (lhs.value: @unchecked) match
          case F(_, Syntax(TermTree.IntegerLiteral(1), _)) => ()

  test("elimination"):
    val optimized = optimize("if true then 1 else 2 * 3")
    (optimized.syntax.value: @unchecked) match
    case TermTree.IntegerLiteral(1) => ()

  test("constant propagation and folding cascade"):
    val optimized = optimize("let x = 2 ; x + 3 * x + 1")
    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(9) => ()

  test("inlining functions"):
    val optimized = optimize("((x: Int) => x + x) 5")

    (optimized.syntax.value : @unchecked) match
      case TermTree.IntegerLiteral(10) => ()

  /** Compiles `input` to a WebAssembly module and returns an instance of it. */
  private def optimize(input: String): TypedProgram =
    Optimizer.optimize(Typer.check(Parser.parse(SourceFile("test", input))))

end OptimizerTests
