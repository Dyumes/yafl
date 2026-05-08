import yafl.SourceFile

final class SourceFileTests extends munit.FunSuite:

  test("lineStarts"):
    val input = SourceFile("test", "abc def\nghi\n!")
    assert(input.lineStarts.sameElements(IArray(0, 8, 12)))

  test("lineContaining"):
    val input = SourceFile("test", "abc def\nghi\n!")
    assertEquals(input.lineContaining(0), 1)
    assertEquals(input.lineContaining(10), 2)
    assertEquals(input.lineContaining(input.end - 1), 3)

  test("lineAndColumn"):
    val input = SourceFile("test", "abc def\nghi\n!")
    assertEquals(input.lineAndColumn(0), SourceFile.LineAndColumn(1, 1))
    assertEquals(input.lineAndColumn(10), SourceFile.LineAndColumn(2, 3))
    assertEquals(input.lineAndColumn(input.end - 1), SourceFile.LineAndColumn(3, 1))

  test("lineContents"):
    val input = SourceFile("test", "abc def\nghi\n")
    assertEquals(input.lineContents(1), "abc def\n")
    assertEquals(input.lineContents(2), "ghi\n")
    assertEquals(input.lineContents(3), "")

  test("apply"):
    val input = SourceFile("test", "abc def\nghi\n")
    assertEquals(input.apply(input.span), input.text)

end SourceFileTests
