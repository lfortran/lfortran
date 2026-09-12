# Assert

Checks a condition at run time.

## Declaration

### Syntax

```text
Assert(expr test, expr? msg)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `test` | the condition that must hold. |
| `msg` | the message reported when it does not, or `nil`. |

### Return values

None.

## Description

**Assert** has no Fortran spelling. It is LPython's `assert`, and it is also
useful to compiler-generated checks: it says that `test` must be true, and
stops the program with `msg` when it is not.

## Examples

```clojure
(Assert
  :test (IntegerCompare
    :left (Var
      :v (SymbolRef 1 "j")
    )
    :op :GtE
    :right (IntegerConstant
      :n 0
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
    :type (Logical
      :kind 4
    )
    :value nil
  )
  :msg (StringConstant
    :s "j is not negative"
    :type (String
      :kind 1
      :len (IntegerConstant
        :n 17
        :type (Integer
          :kind 4
        )
        :intboz_type :Decimal
      )
      :len_kind :ExpressionLength
      :physical_type :DescriptorString
    )
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/control_stmt.asr
:language: clojure
```

## See Also

[ErrorStop](ErrorStop.md), [If](If.md)
