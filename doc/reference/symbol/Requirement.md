# Requirement

Requirement, a **symbol** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
Requirement(symbol_table symtab, identifier name, identifier* args, require_instantiation* requires)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `symtab` of type `symbol_table`, `name` of type `identifier`, `args` of type `identifier*`, `requires` of type `require_instantiation*`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## ASR

_No ASR example yet._

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* The Requirement::m_symtab cannot be nullptr
* The Requirement::m_symtab->parent is not the right parent
* Requirement::m_symtab->counter must be unique
* The X::m_symtab::asr_owner must point to X
* The asr_owner invariant failed
<!-- END AUTO: restrictions -->
