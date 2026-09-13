# StructMethodDeclaration

A type-bound procedure of a derived type.

## Declaration

### Syntax

```text
StructMethodDeclaration(symbol_table parent_symtab, identifier name,
    identifier? self_argument, identifier proc_name, symbol proc,
    abi abi, bool is_deferred, bool is_nopass)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `parent_symtab` | the symbol table of the derived type that declares the binding. |
| `name` | the binding name, the name written after the `%`. |
| `self_argument` | the name of the passed-object dummy argument, or `nil` for the first argument. |
| `proc_name` | the name of the procedure the binding resolves to. |
| `proc` | the procedure symbol itself. |
| `abi` | the ABI of the procedure. |
| `is_deferred` | `true` for a `deferred` binding of an abstract type, which has no implementation here. |
| `is_nopass` | `true` for `nopass`: the object is not passed as an argument. |

### Return values

None.

## Description

A **StructMethodDeclaration** is stored in the symbol table of the
[Struct](Struct.md) that declares it, and it names the procedure that
implements the binding. Binding name and procedure name are separate, because
`procedure :: area => circle_area` gives them different spellings.

A call through a binding is an ordinary
[SubroutineCall](../statement_nodes/SubroutineCall.md) or
[FunctionCall](../expression_nodes/FunctionCall.md) whose `dt` member carries
the object the binding was reached through. For a `deferred` binding of an
abstract type the actual procedure is chosen at run time from the dynamic type
of `dt`.

## Examples

```clojure
(StructMethodDeclaration
  :parent_symtab 2
  :name "area"
  :self_argument nil
  :proc_name "circle_area"
  :proc (SymbolRef 1 "circle_area")
  :abi :Source
  :is_deferred false
  :is_nopass false
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/structmethoddeclaration.asr
:language: clojure
```

## See Also

[Struct](Struct.md), [FunctionCall](../expression_nodes/FunctionCall.md), [SubroutineCall](../statement_nodes/SubroutineCall.md), [Function](Function.md)
