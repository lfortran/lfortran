# AssociateBlock

AssociateBlock, a **symbol** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
AssociateBlock(symbol_table symtab, identifier name, stmt* body)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `symtab` of type `symbol_table`, `name` of type `identifier`, `body` of type `stmt*`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## ASR

_No ASR example yet._

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* The AssociateBlock::m_symtab cannot be nullptr
* The AssociateBlock::m_symtab->parent is not the right parent
* AssociateBlock::m_symtab->counter must be unique
* The X::m_symtab::asr_owner must point to X
* The asr_owner invariant failed
<!-- END AUTO: restrictions -->
