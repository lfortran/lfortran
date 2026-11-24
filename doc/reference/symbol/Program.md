<!-- This is an automatically generated file. Do not edit it manually. -->

# Program

Program, a **symbol** node.

## Declaration

### Syntax

Program(symbol_table symtab, identifier name, identifier* dependencies, stmt* body, location start_name, location end_name)

### Arguments
Input arguments are `symtab` of type symbol_table, `name` of type identifier, `dependencies` of type identifier*, `body` of type stmt*, `start_name` of type location, `end_name` of type location.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* The Program::m_symtab cannot be nullptr
* The Program::m_symtab->parent is not the right parent
* The Program::m_symtab's parent must be TranslationUnit
* Program::m_symtab->counter must be unique
* The X::m_symtab::asr_owner must point to X
* The asr_owner invariant failed
* Program name is required
* ::m_dependencies is required