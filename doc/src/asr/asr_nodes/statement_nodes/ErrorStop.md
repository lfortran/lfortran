# ErrorStop

Stops the program with an error status.

## Declaration

### Syntax

```text
ErrorStop(expr? code)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `code` | the stop code, or `nil`. An integer becomes the exit status and a character value is written to the error unit. |

### Return values

None.

## Description

`error stop` terminates the whole program, and unlike [Stop](Stop.md) it is an
error termination: no further output is flushed in an orderly way and, in a
coarray program, the other images are terminated as well.

## Examples

```clojure
(ErrorStop
  :code (IntegerConstant
    :n 1
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/stop_stmt.asr
:language: clojure
```

## See Also

[Stop](Stop.md), [Assert](Assert.md)
