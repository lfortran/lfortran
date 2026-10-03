# Stop

Stops the program normally.

## Declaration

### Syntax

```text
Stop(expr? code)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `code` | the stop code, or `nil`. An integer becomes the exit status; a character value is written to the error unit. |

### Return values

None.

## Description

`stop` terminates the program in an orderly way: units are flushed and
closed. [ErrorStop](ErrorStop.md) is the error termination counterpart.

## Examples

```clojure
(Stop
  :code (IntegerConstant
    :n 0
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

[ErrorStop](ErrorStop.md), [Return](Return.md)
