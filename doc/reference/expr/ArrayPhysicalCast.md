# ArrayPhysicalCast

## ASR

<!-- BEGIN AUTO: asr -->
```
ArrayPhysicalCast(expr arg, array_physical_type old, array_physical_type new, ttype type, expr? value)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* ArrayPhysicalCast is redundant, the old physical type and new physical type must be different.
* Destination physical type conflicts with the physical type of target
* Old physical type conflicts with the physical type of argument [...] [...]
<!-- END AUTO: verify -->
