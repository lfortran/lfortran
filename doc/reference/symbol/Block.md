<!-- This is an automatically generated file. Do not edit it manually. -->

# Block

Block, a **symbol** node.

## Declaration

### Syntax

Block(symbol_table symtab, identifier name, stmt* body)

### Arguments
Input arguments are `symtab` of type symbol_table, `name` of type identifier, `body` of type stmt*.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* The AssociateBlock::m_symtab cannot be nullptr
* The AssociateBlock::m_symtab->parent is not the right parent
* AssociateBlock::m_symtab->counter must be unique
* The X::m_symtab::asr_owner must point to X
* The asr_owner invariant failed