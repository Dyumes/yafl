import com.dylibso.chicory
import com.dylibso.chicory.tools.wasm.Wat2Wasm

import java.io.File

import yafl.SourceFile
import yafl.emitter.Emitter
import yafl.parser.Parser
import yafl.typer.Typer

final class EmitterTests extends munit.FunSuite:

  test("integer addition"):
    val input = SourceFile("test", "40 + 2")
    val wasm = Wat2Wasm.parse(compile(input))
    val m = chicory.wasm.Parser.parse(wasm)
    val i = chicory.runtime.Instance.builder(m).build()
    val f = i.`export`("main")
    assertEquals(f.apply()(0), 42L)

  /** Parses and type check the program in `input`. */
  private def compile(input: SourceFile): String =
    val program = Typer.check(Parser.parse(input))
    Emitter.emit(program)

end EmitterTests
