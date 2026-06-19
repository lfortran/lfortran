<!-- This is an automatically generated file. Do not edit it manually. -->

# Template

Template, a **symbol** node.

## Declaration

### Syntax

Template(symbol_table symtab, identifier name, identifier* args, require_instantiation* requires)

### Arguments
Input arguments are `symtab` of type symbol_table, `name` of type identifier, `args` of type identifier*, `requires` of type require_instantiation*.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* The Requirement::m_symtab cannot be nullptr
* The Requirement::m_symtab->parent is not the right parent
* Requirement::m_symtab->counter must be unique
* The X::m_symtab::asr_owner must point to X
* The asr_owner invariant failed