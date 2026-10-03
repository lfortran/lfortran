# IfExp

## ASR

<!-- BEGIN AUTO: asr -->
```
IfExp(expr test, expr body, expr orelse, ttype type, expr? value)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* IfExp condition must be logical
* IfExp condition must be a scalar
* IfExp arms must have the same type and kind, found [...] and [...]
* IfExp arms must have the same rank
* IfExp result must have the same rank as its arms
<!-- END AUTO: verify -->
