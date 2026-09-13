# Program

The main program: the entry point of an executable.

## Declaration

### Syntax

```text
Program(symbol_table symtab, identifier name, identifier* dependencies,
    stmt* body, location start_name, location end_name)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `symtab` | the symbol table of the program. It owns the local variables of the program and the `ExternalSymbol` entries for the module symbols the program uses. |
| `name` | the name of the program. |
| `dependencies` | the names of the modules and procedures the body of the program refers to. The backends use it to order code generation. |
| `body` | the statements of the program, in order. |
| `start_name` | the source span of the name in `program name`. |
| `end_name` | the source span of the name in `end program name`, or an empty span when the end statement does not repeat it. |

### Return values

None.

## Description

A translation unit that is linked into an executable must contain exactly one
**Program**. It is always owned by the global symbol table.

A program has no arguments and no return value, so it needs no
`function_signature`: unlike [Function](Function.md) it can never be called
from ASR.

Statements typed directly into the REPL, or written outside of any program
unit, first appear in `TranslationUnit.items`. The `global_stmts` ASR pass
wraps them in a **Program** so that the rest of the compiler only has to deal
with program units.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/program.asr
:language: clojure
```

## See Also

[TranslationUnit](../unit_nodes/TranslationUnit.md), [Module](Module.md), [Function](Function.md), [Variable](Variable.md)
