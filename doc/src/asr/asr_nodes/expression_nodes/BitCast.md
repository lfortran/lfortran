# BitCast

Reinterprets the bits of a value as another type.

## Declaration

### Syntax

```text
BitCast(expr source, expr mold, expr? size, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `source` | the value whose bits are taken. |
| `mold` | a value of the target type; it is not read, only its type is used. |
| `size` | the number of elements of the result, when it is an array; `nil` otherwise. |
| `type` | the type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`transfer(source, mold)`. Nothing is converted: the bits are copied and read
as the new type, so transferring a `real(4)` to an `integer(4)` gives its
representation rather than its value. [Cast](Cast.md) is the node that
converts.

## Examples

```clojure
(BitCast
  :source (Var
    :v (SymbolRef 1 "x")
  )
  :mold (Var
    :v (SymbolRef 1 "i")
  )
  :size nil
  :type (Integer
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/bitcast.asr
:language: clojure
```

## See Also

[Cast](Cast.md), [SizeOfType](SizeOfType.md), [CLoc](CLoc.md)
