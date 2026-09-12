# CompilerOptions

The compiler options the translation unit was compiled with.

## Declaration

### Syntax

```text
CompilerOptions(string compiler_options_str, ttype type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `compiler_options_str` | the options, as one string. |
| `type` | the string type of the result. |

### Return values

The value of the expression.

## Description

`compiler_options()` is an LFortran extension that lets a program report how
it was built. The value is fixed when the ASR is produced, so the node is a
constant; it is a node of its own rather than a
[StringConstant](StringConstant.md) so that unparsed ASR still shows the call
rather than the expanded text.

## Examples

```clojure
(CompilerOptions
  :compiler_options_str "-O3 --fast-math"
  :type (String
    :kind 1
    :len (IntegerConstant
      :n 15
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
    :len_kind :ExpressionLength
    :physical_type :DescriptorString
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/compileroptions.asr
:language: clojure
```

## See Also

[StringConstant](StringConstant.md), [String](../type_nodes/String.md)
