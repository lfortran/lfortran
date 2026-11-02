# ScopedVariableAttribute

A scoped VOLATILE or ASYNCHRONOUS declaration for a host- or use-associated
variable.

## Declaration

### Syntax

```text
ScopedVariableAttribute(symbol_table parent_symtab, identifier name,
    symbol variable, bool is_volatile, bool is_asynchronous)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `parent_symtab` | the symbol table of the scope declaring the attributes. |
| `name` | an internal symbol-table key, not a Fortran entity name. |
| `variable` | the visible host variable or its local [ExternalSymbol](ExternalSymbol.md), including a use rename. Following external symbols identifies the canonical [Variable](Variable.md). |
| `is_volatile` | the variable is VOLATILE in this scope. |
| `is_asynchronous` | the variable is ASYNCHRONOUS in this scope. |

### Return values

None.

## Description

At least one attribute must be true. The reference must be visible from the
declaring scope and identify a variable defined outside that scope. Attributes
on a locally defined variable are recorded on that Variable instead. A later
local declaration that shadows a host variable removes the tentative scoped
declaration and attaches the attribute to the local variable.

This declaration changes neither the canonical entity nor other scopes that
use it. It is not exported by USE or submodule imports. Its internal name must
not appear in regenerated Fortran: the printer emits attribute statements using
the visible variable name in the declaring specification part.

The reference describes entity identity, not a runtime access. Expression
lowering must not replace it with a context-storage alias. Symbol duplication
and template instantiation remap it to the corresponding entity in copied
scopes, retaining shared host-module storage. Module serialization preserves
the reference; verification checks visibility before external symbols are
resolved, then checks the canonical variable after resolution.

Alias analysis and dead-store elimination must account for these scoped facts
as well as the canonical variable's attributes, including contained scopes.

## Examples

In the following example, `a` remains an ordinary dummy outside the BLOCK;
the BLOCK's specification part owns its ASYNCHRONOUS declaration.

```fortran
subroutine scoped_roundtrip(a)
    real :: a(4)
    block
        asynchronous :: a
        a = 1
    end block
end subroutine

program scoped_attributes
    real :: a(4)
    call scoped_roundtrip(a)
    if (any(a /= 1)) error stop
end program
```

Its complete ASR is:

```{literalinclude} ../../examples/scoped_variable_attribute.asr
:language: clojure
```

## See Also

[Variable](Variable.md), [ExternalSymbol](ExternalSymbol.md), [Block](Block.md)
