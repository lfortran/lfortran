# ModularInteger

Represents modular integer, a `ttype` node

## Declaration

### Syntax

```fortran
ModularInteger(int modulus, dimension* dims)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`modulus`| modulus, such as 2^16 |
|`dims`| optional dimensions for an array |

## Description

**ModularInteger** represents a modular integer of a given modulus.

Currently only 2^8, 2^16, 2^32 and 2^64 moduli are implemented and are
implemented in hardware as 8, 16, 32 and 64 bit unsigned integers.

For example the `(ModularInteger 65536 [])` represent values 0, 1, ..., 65535,
and is implemented as a 16bit unsigned integer in hardware.

Regular `Integer` is signed and an overflow is a runtime error in Debug mode
(and undefined in Release mode). The `ModularInteger` cannot overflow, it wraps
around instead using the modulo operation.

`ModularInteger` cannot be used as a loop variable. When used in `BindC`
context, `ModularInteger` is mapped to C unsigned integers.

# Operations

The operations defined on modular integers are:

* Binary arithmetic operations (`+`, `-`, `*`, `/`, `**`) and unary minus. The
  operations are define as if on signed integers of infinite size, and then the
  modulo is taken, so that the value is between 0..modulus-1. Both arguments
  must be `ModularInteger` and the result is a `ModularInteger`.
* Comparison operations: `<`, `>`, `==`, `>=`, `<=`, `/=`, defined as if first
  casting to signed integers of infinite size and comparing. The result is a
  `Logical`.
* Casting to and from `BitVector` and `Integer`. The casting does not change
  bit content, just interpretation.

Bitwise operations are not supported (cast to `BitVector` for that).

# When to use

When trying to use finite representation for a mathematical integer, use
`Integer`. The overflow is a runtime error (in Debug mode), if it happens,
increase the size.

When needing integers that wrap around and overflow cannot occur and that are
tied to their bit representation, use `ModularInteger` with `modulus=2^n_bits`.
For example if you need a 5bit integer that wraps around, use
`(ModularInteger 2^5 [])`.

When needing to do bitwise operations, use or cast to `BitVector`.

You can always cast between the three representations without any runtime
overhead, so choose your main representation based on what you need the most
often.


## Examples

Python code:

```python
x: u16
y: u64
```

ASR:

```clojure
(TranslationUnit
    (SymbolTable
        1
        {
            :x
                (Variable
                    1
                    x
                    []
                    Local
                    ()
                    ()
                    Default
                    (ModularInteger 65536 [])
                    Source
                    Public
                    Required
                    .false.
                ),
            :y
                (Variable
                    1
                    y
                    []
                    Local
                    ()
                    ()
                    Default
                    (ModularInteger 18446744073709551616 [])
                    Source
                    Public
                    Required
                    .false.
                )
        })
    []
)
```

## See Also

[LogicalNot](logicalnot.md)
