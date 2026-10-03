# CoarrayRef

## ASR

<!-- BEGIN AUTO: asr -->
```
CoarrayRef(expr var, coarray_index* coindices, ttype type, expr? value)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* coarray_index_t with star must have `nullptr` index
* coarray_index_t with star may only appear in the final codimension
* coarray_index_t without star must have a valid index
<!-- END AUTO: verify -->
