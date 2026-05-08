package yafl.typer

import yafl.syntax.{Syntax, TermTree}

/** The syntax tree of a program along with the types of each term. */
case class TypedProgram(syntax: Syntax[TermTree], types: Map[Syntax[TermTree], Type]):

  /** Returns the type of `term`. */
  def typeOf(term: Syntax[TermTree]): Type =
    types(term)

end TypedProgram
