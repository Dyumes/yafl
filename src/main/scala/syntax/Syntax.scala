package yafl.syntax

import yafl.SourceSpan
import yafl.typer.Type

/** A syntax tree representing a term or a type.
  *
  * @param value The payload of the tree.
  * @param span The site from which the tree was parsed.
  */
case class Syntax[+T](val value: T, val span: SourceSpan):

  override def toString(): String =
    value.toString()

end Syntax
