package yafl

import java.io.FileNotFoundException
import scala.io.AnsiColor.{BOLD, GREEN, RED, RESET}

import yafl.parser.Parser
import yafl.typer.Typer

/** The entry point of the application.
 *
 *  @param filepath The path to the input file.
 */
@main def Main(filepath: String): Unit =
  try
    // Load the source file from disk.
    val source = SourceFile.contentsOf(filepath)

    // Parse and type check the program.
    val program = Parser.parse(source)
    Typer.check(program)(using Typer.Context.builtin)

    println(program)
  catch
    case e: FileNotFoundException => error(None, s"no such file: ${e.getMessage()}")
    case e: Diagnostic => render(e)
    case e => error(None, e.toString)

/** Logs a success to the standard output. */
def success(message: String): Unit =
  println(s"${GREEN}${BOLD}success:${RESET} ${message}")

/** Logs an error to the standard error at `location`. */
def error(location: Option[SourceSpan], message: String): Unit =
  location match
    case Some(l) =>
      System.err.println(s"${l}: ${RED}${BOLD}error:${RESET} ${message}")
    case _ =>
      System.err.println(s"${RED}${BOLD}error:${RESET} ${message}")

/** Logs a diagnostic to the standard error. */
def render(d: Diagnostic): Unit = {
  // Log the message of the diagnostic.
  error(Some(d.span), d.description)

  // Log the line at which the diagnostic occurred.
  val source = d.span.source
  val text = source.lineContents(source.lineContaining(d.span.start) )
  System.err.println(text)

  // Log the column atwhich the diansotic occurred.
  val location = source.lineAndColumn(d.span.start)
  System.err.print(" " * (location.column - 1))
  System.err.println("^")
}
