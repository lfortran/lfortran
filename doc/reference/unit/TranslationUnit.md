<!-- This is an automatically generated file. Do not edit it manually. -->

# TranslationUnit

TranslationUnit, a **unit** node.

## Declaration

### Syntax

TranslationUnit(symbol_table symtab, node* items)

### Arguments
Input arguments are `symtab` of type symbol_table, `items` of type node*.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* The TranslationUnit::m_symtab cannot be nullptr
* The TranslationUnit::m_symtab->parent must be nullptr
* TranslationUnit::m_symtab->counter must be unique
* The TranslationUnit::m_symtab::asr_owner must point to itself
* The asr_owner invariant failed
* TranslationUnit::m_items must be either stmt or expr