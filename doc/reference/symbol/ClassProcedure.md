<!-- This is an automatically generated file. Do not edit it manually. -->

# ClassProcedure

ClassProcedure, a **symbol** node.

## Declaration

### Syntax

ClassProcedure(symbol_table parent_symtab, identifier name, identifier? self_argument, identifier proc_name, symbol proc, abi abi, bool is_deferred, bool is_nopass)

### Arguments
Input arguments are `parent_symtab` of type symbol_table, `name` of type identifier, `self_argument` of type identifier?, `proc_name` of type identifier, `proc` of type symbol, `abi` of type abi, `is_deferred` of type bool, `is_nopass` of type bool.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* The ClassProcedure::m_name cannot be nullptr
* The ClassProcedure::m_proc cannot be nullptr
* The ClassProcedure::m_proc_name cannot be nullptr
* ClassProcedure::m_parent_symtab cannot be nullptr
* ClassProcedure not found in parent_symtab symbol table
* ClassProcedure's parent symbol table does not point to it
* ClassProcedure::m_parent_symtab must be present in the ASR ()
* must be present inprocedures.