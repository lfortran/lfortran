# Print

Writes a formatted record to standard output.

## Declaration

### Syntax

```text
Print(expr text)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `text` | the text to write, normally a [StringFormat](../expression_nodes/StringFormat.md) holding the format and the output list. |

### Return values

None.

## Description

**Print** carries a single expression rather than a format and a list of
values: formatting is
[StringFormat](../expression_nodes/StringFormat.md)'s job, so a backend that
can produce a string already has everything it needs to implement `print`.

`print *, x` is a **Print** whose `text` is a **StringFormat** with a `nil`
format, meaning list-directed output.

## Examples

```clojure
(Print
  :text (StringFormat
    :fmt nil
    :args [
      (Var
        :v (SymbolRef 1 "n")
      )
    ]
    :kind :FormatFortran
    :type (String
      :kind 1
      :len nil
      :len_kind :DeferredLength
      :physical_type :DescriptorString
    )
    :value nil
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/print_stmt.asr
:language: clojure
```

## See Also

[StringFormat](../expression_nodes/StringFormat.md), [FileWrite](FileWrite.md)
