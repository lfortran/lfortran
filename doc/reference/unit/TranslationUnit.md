# TranslationUnit

## ASR

<!-- BEGIN AUTO: asr -->
```
TranslationUnit(symbol_table symtab, node* items)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* The `TranslationUnit::m_symtab` cannot be `nullptr`
* The `TranslationUnit::m_symtab`->parent must be `nullptr`
* `TranslationUnit::m_symtab`->counter must be unique
* The `TranslationUnit::m_symtab::asr_owner` must point to itself
* The asr_owner invariant failed
* `TranslationUnit::m_items` must be either stmt or expr
<!-- END AUTO: verify -->
