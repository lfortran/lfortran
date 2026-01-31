<!-- This is an automatically generated file. Do not edit it manually. -->
# ExternalSymbol

ExternalSymbol, a **symbol** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
ExternalSymbol(symbol_table parent_symtab, identifier name, symbol external, identifier module_name, identifier* scope_names, identifier original_name, access access)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `parent_symtab` of type `symbol_table`, `name` of type `identifier`, `external` of type `symbol`, `module_name` of type `identifier`, `scope_names` of type `identifier*`, `original_name` of type `identifier`, `access` of type `access`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* `ExternalSymbol::m_external` cannot be `nullptr`
* `ExternalSymbol::m_external` cannot be an ExternalSymbol
* `ExternalSymbol::m_original_name` must match external->`m_name`
* `ExternalSymbol::m_external` '[...]' is not in a module or struct type, owner: [...]
* `ExternalSymbol::m_module_name` `[...]` must match external's module name `[...]`
* `ExternalSymbol::m_original_name` ('[...]') + scope_names not found in a module '[...]'
* `ExternalSymbol::m_name` + scope_names found but not equal to `m_external`, [...]original_name [...].
<!-- END AUTO: restrictions -->
