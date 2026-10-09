# StructMethodDeclaration

A type-bound procedure of a derived type.

## Declaration

### Syntax

```text
StructMethodDeclaration(symbol_table parent_symtab, identifier name,
    identifier? self_argument, identifier proc_name, symbol proc,
    abi abi, bool is_deferred, bool is_nopass, symbol? dispatch_proc)
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
| `dispatch_proc` | optional typed adapter for the inherited virtual slot of a sealed nonpolymorphic override; `proc` retains the source implementation identity. |

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

For a sealed override whose source receiver is `TYPE(T)` but whose inherited
slot takes `CLASS(Parent)`, `dispatch_proc` is a private, typed forwarding entry
with a `CLASS(T)` receiver and an explicit `ClassToStruct` conversion. Static
calls and nominal conformance still use `proc`. The adapter forwards OUT dummies
as INOUT so the real implementation alone performs entry cleanup. Its scope
contains only signature declarations, not copied implementation locals.
The verifier requires the adapter for this ABI change and checks its ownership,
nominal receiver, signature and dummy attributes. Current result support is
limited to scalar numeric/logical values; lifetime-bearing results require
additional transparent result-slot lowering.

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
  :dispatch_proc nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/structmethoddeclaration.asr
:language: clojure
```

## See Also

[Struct](Struct.md), [FunctionCall](../expression_nodes/FunctionCall.md), [SubroutineCall](../statement_nodes/SubroutineCall.md), [Function](Function.md)
