# Yafl – Yet Another Functional Language

A toy implementation of a tiny programming language.

## Syntax

The syntax of Yafl is defined below.
Identifers are strings of alphanumeric characters and the underscore, starting with a non-numeric character (e.g., foo or _23).

```
term ::=
  | identifier
  | term-abstraction
  | term-application
  | type-abstraction
  | type-application

term-abstraction ::=
  | '(' identifier ':' type (',' identifier ':' type)* ')' '=>' term

term-application ::=
  | term term

type-abstraction ::=
  | '[' identifier (',' identifier)* ']' '=>' term

type-application ::=
  | term '[' type (',' type)* ']'

type ::=
  | identifier
  | arrow
  | forall
  | '_'

type arrow ::=
  | type -> type

type forall ::=
  | '[' identifier (',' identifier)* ']' => type
```
