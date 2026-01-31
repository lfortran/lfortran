# Module

Module, a **symbol** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
Module(symbol_table symtab, identifier name, identifier? parent_module, identifier* dependencies, bool loaded_from_mod, bool intrinsic, bool has_submodules, location start_name, location end_name)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `symtab` of type `symbol_table`, `name` of type `identifier`, `parent_module` of type `identifier?`, `dependencies` of type `identifier*`, `loaded_from_mod` of type `bool`, `intrinsic` of type `bool`, `has_submodules` of type `bool`, `start_name` of type `location`, `end_name` of type `location`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## ASR

_No ASR example yet._

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* The Module::m_symtab cannot be nullptr
* The Module::m_symtab->parent is not the right parent
* The Module::m_symtab's parent must be TranslationUnit
* Module::m_symtab->counter must be unique
* The X::m_symtab::asr_owner must point to X
* Module name is required
* The asr_owner invariant failed
* A module dependency must not be a nullptr
* A module dependency must not be an empty string
* A module dependency must be a valid string
* Module [...] dependencies must contain [...] because a function present in it is getting called in [...].
<!-- END AUTO: restrictions -->
