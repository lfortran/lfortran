# ArrayConstructor

## ASR

<!-- BEGIN AUTO: asr -->
```
ArrayConstructor(expr* args, ttype type, expr? value, arraystorage storage_format, expr? struct_var)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* Type of ArrayConstructor must be an array
* `ArrayConstructor::m_struct_vars` must be `nullptr` or var to struct symbol
<!-- END AUTO: verify -->
