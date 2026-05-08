import yafl.SourceFile
import yafl.parser.Parser
import yafl.syntax.{Syntax, TermTree}
import yafl.typer.{Type, TypedProgram, Typer}

final class TyperTests extends munit.FunSuite:

  test("integer literal"):
    val input = SourceFile("test", "42")
    val program = parseAndTypeCheck(input)
    assertEquals(program.typeOf(program.syntax), Type.Ground.Int)

  /** Parses and type check the program in `input`. */
  private def parseAndTypeCheck(input: SourceFile): TypedProgram =
    val untyped = Parser.parse(input)
    Typer.check(untyped)

end TyperTests
