# AssociateBlock

## ASR

<!-- BEGIN AUTO: asr -->
```
AssociateBlock(symbol_table symtab, identifier name, stmt* body)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* The `AssociateBlock::m_symtab` cannot be `nullptr`
* The `AssociateBlock::m_symtab`->parent is not the right parent
* `AssociateBlock::m_symtab`->counter must be unique
* The `X::m_symtab::asr_owner` must point to X
* The asr_owner invariant failed
<!-- END AUTO: verify -->
