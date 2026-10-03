# ExternalSymbol

## ASR

<!-- BEGIN AUTO: asr -->
```
ExternalSymbol(symbol_table parent_symtab, identifier name, symbol external, identifier module_name, identifier* scope_names, identifier original_name, access access)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* `ExternalSymbol::m_external` cannot be `nullptr`
* `ExternalSymbol::m_external` cannot be an ExternalSymbol
* `ExternalSymbol::m_original_name` must match external->`m_name`
* `ExternalSymbol::m_external` '[...]' is not in a module or struct type, owner: [...]
* `ExternalSymbol::m_module_name` `[...]` must match external's module name `[...]`
* `ExternalSymbol::m_original_name` ('[...]') + scope_names not found in a module '[...]'
* `ExternalSymbol::m_name` + scope_names found but not equal to `m_external`, [...]original_name [...].
<!-- END AUTO: verify -->
