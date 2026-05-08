import yafl.SourceFile
import yafl.parser.Parser
import yafl.syntax.TermTree

final class ParserTests extends munit.FunSuite:

  test("integer literal"):
    val input = SourceFile("test", "42")
    Parser.parse(input).value match
      case TermTree.IntegerLiteral(n) =>
        assertEquals(n, 42)
      case e =>
        assert(false, s"expected integer literal, found '${e}'")

end ParserTests
