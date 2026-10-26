# GenericProcedure

A name that resolves to one of several procedures.

## Declaration

### Syntax

```text
GenericProcedure(symbol_table parent_symtab, identifier name,
    symbol* procs, access access)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `parent_symtab` | the symbol table this symbol is stored in. |
| `name` | the generic name. |
| `procs` | the specific procedures the name can resolve to. |
| `access` | `Public` or `Private`. |

### Return values

None.

## Description

A **GenericProcedure** is the ASR form of a generic interface block and of a
generic type-bound procedure. It is a symbol, not a call: resolution happens
in the frontend, which picks the specific procedure from the actual arguments
and stores it in the `name` member of the call node. The generic symbol is
kept in `original_name` there, so a later pass can still tell that the call was
written generically.

A **GenericProcedure** is never called directly and never reaches a backend as
a call target.

### A generic procedure imported by `use`

`use` does not import a generic procedure or an operator
([CustomOperator](CustomOperator.md)) of a module as an
[ExternalSymbol](ExternalSymbol.md). The using scope gets its own
**GenericProcedure** under the local name of the generic. Each specific
procedure of the module's generic, public or private, becomes a private
**ExternalSymbol** of the using scope named `<specific>@<generic>`, where
`<specific>` is the name of the procedure in its module and `<generic>` the
local name of the generic. These imports are the `procs` of the new node. A
number is added to the name when another procedure has it, for example when
two modules have private specifics of the same name.

```fortran
module m
    implicit none
    private
    public :: show
    interface show
        module procedure show_int, show_real
    end interface
contains
    ...
end module

program p
    use m, only: print_it => show
    ...
end program
```

The scope of `p` gets:

| Name | Symbol |
|------|--------|
| `print_it` | a GenericProcedure whose `procs` are `show_int@print_it` and `show_real@print_it` |
| `show_int@print_it` | a private ExternalSymbol to `show_int` of `m` |
| `show_real@print_it` | a private ExternalSymbol to `show_real` of `m` |

A call through the generic takes the specific from `procs`; it never looks a
procedure up by name. So a procedure of the program or of its host with the
name of a specific does not take its place. When the scope already has a
generic of that name, from another `use` or from an interface block, the
specifics it lacks are added to it (F2018 C1515). A module passes an imported
generic on like its own one, unless the generic is private there.

A type-bound generic comes with its type, not through `use`. A call through it
imports the specific procedure at the call, under the same
`<specific>@<generic>` name.

## Examples

```clojure
(GenericProcedure
  :parent_symtab 1
  :name "show"
  :procs [
    (SymbolRef 1 "show_int")
    (SymbolRef 1 "show_real")
  ]
  :access :Public
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/genericprocedure.asr
:language: clojure
```

## See Also

[CustomOperator](CustomOperator.md), [Function](Function.md), [FunctionCall](../expression_nodes/FunctionCall.md), [SubroutineCall](../statement_nodes/SubroutineCall.md)
