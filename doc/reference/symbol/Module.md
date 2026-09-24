# Module

## ASR

<!-- BEGIN AUTO: asr -->
```
Module(symbol_table symtab, identifier name, identifier? parent_module, identifier* dependencies, bool loaded_from_mod, bool intrinsic, bool has_submodules, location start_name, location end_name)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* The `Module::m_symtab` cannot be `nullptr`
* The `Module::m_symtab`->parent is not the right parent
* The `Module::m_symtab`'s parent must be TranslationUnit
* `Module::m_symtab`->counter must be unique
* The `X::m_symtab::asr_owner` must point to X
* Module name is required
* The asr_owner invariant failed
* A module dependency must not be a `nullptr`
* A module dependency must not be an empty string
* A module dependency must be a valid string
* Module [...] dependencies must contain [...] because a function present in it is getting called in [...].
<!-- END AUTO: verify -->
