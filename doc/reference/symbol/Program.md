# Program

## ASR

<!-- BEGIN AUTO: asr -->
```
Program(symbol_table symtab, identifier name, identifier* dependencies, stmt* body, location start_name, location end_name)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* The `Program::m_symtab` cannot be `nullptr`
* The `Program::m_symtab`->parent is not the right parent
* The `Program::m_symtab`'s parent must be TranslationUnit
* `Program::m_symtab`->counter must be unique
* The `X::m_symtab::asr_owner` must point to X
* The asr_owner invariant failed
* Program name is required
* ::`m_dependencies` is required
<!-- END AUTO: verify -->
