# Template

## ASR

<!-- BEGIN AUTO: asr -->
```
Template(symbol_table symtab, identifier name, identifier* args, require_instantiation* requires)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* The `Requirement::m_symtab` cannot be `nullptr`
* The `Requirement::m_symtab`->parent is not the right parent
* `Requirement::m_symtab`->counter must be unique
* The `X::m_symtab::asr_owner` must point to X
* The asr_owner invariant failed
<!-- END AUTO: verify -->
