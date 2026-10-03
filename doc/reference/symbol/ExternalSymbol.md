<!-- This is an automatically generated file. Do not edit it manually. -->

# ExternalSymbol

ExternalSymbol, a **symbol** node.

## Declaration

### Syntax

ExternalSymbol(symbol_table parent_symtab, identifier name, symbol external, identifier module_name, identifier* scope_names, identifier original_name, access access)

### Arguments
Input arguments are `parent_symtab` of type symbol_table, `name` of type identifier, `external` of type symbol, `module_name` of type identifier, `scope_names` of type identifier*, `original_name` of type identifier, `access` of type access.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* ExternalSymbol::m_external cannot be nullptr
* ExternalSymbol::m_external cannot be an ExternalSymbol
* ExternalSymbol::m_original_name must match external->m_name
* ExternalSymbol::m_external is not in a module or struct type, owner:
* ExternalSymbol::m_module_name `` must match external's module name ``
* ExternalSymbol::m_original_name () + scope_names not found in a module
* ExternalSymbol::m_name + scope_names found but not equal to m_external,original_name.