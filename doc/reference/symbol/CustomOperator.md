<!-- This is an automatically generated file. Do not edit it manually. -->

# CustomOperator

CustomOperator, a **symbol** node.

## Declaration

### Syntax

CustomOperator(symbol_table parent_symtab, identifier name, symbol* procs, access access)

### Arguments
Input arguments are `parent_symtab` of type symbol_table, `name` of type identifier, `procs` of type symbol*, `access` of type access.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* CustomOperator::m_name cannot be nullptr
* ::m_parent_symtab cannot be nullptr