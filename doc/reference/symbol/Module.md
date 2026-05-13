<!-- This is an automatically generated file. Do not edit it manually. -->

# Module

Module, a **symbol** node.

## Declaration

### Syntax

Module(symbol_table symtab, identifier name, identifier* dependencies, symbol? parent_module, bool loaded_from_mod, bool intrinsic, location start_name, location end_name)

### Arguments
Input arguments are `symtab` of type symbol_table, `name` of type identifier, `dependencies` of type identifier*, `parent_module` of type symbol?, `loaded_from_mod` of type bool, `intrinsic` of type bool, `start_name` of type location, `end_name` of type location.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
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
* Moduledependencies must containbecause a function present in it is getting called in.