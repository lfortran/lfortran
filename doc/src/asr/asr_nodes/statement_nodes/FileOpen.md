# FileOpen

Connects a unit to a file.

## Declaration

### Syntax

```text
FileOpen(int label, expr? newunit, expr? filename, expr? status,
    expr? form, expr? access, expr? iostat, expr? iomsg, expr? action,
    expr? delim, expr? recl, expr? position, expr? blank,
    expr? encoding, expr? sign, expr? decimal, expr? round, expr? pad,
    expr? asynchronous)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `label` | the statement label of the Fortran statement, or `-1` when it has none. |
| `newunit` | a variable receiving a unit number chosen by the runtime, for `newunit=`. |
| `filename` | the name of the file to connect. |
| `status` | `old`, `new`, `replace`, `scratch` or `unknown`. |
| `form` | `formatted` or `unformatted`. |
| `access` | `sequential`, `direct` or `stream`. |
| `iostat` | a variable receiving the I/O status: zero on success, non-zero otherwise. The statement does not abort when it is present. |
| `iomsg` | a variable receiving the error message when the operation fails. |
| `action` | `read`, `write` or `readwrite`. |
| `delim` | the delimiter used around character values in list-directed and namelist output. |
| `recl` | the record length of a direct access file. |
| `position` | `asis`, `rewind` or `append`. |
| `blank` | `null` or `zero`: how blanks in numeric input are read. |
| `encoding` | `utf-8` or `default`. |
| `sign` | `plus`, `suppress` or `processor_defined`. |
| `decimal` | `point` or `comma`: the decimal edit mode. |
| `round` | the rounding mode for formatted conversion. |
| `pad` | `yes` or `no`: whether short input records are padded with blanks. |
| `asynchronous` | `yes` or `no`: whether asynchronous transfer is allowed on this unit. |

### Return values

None.

## Description

Every specifier of the `open` statement has its own member, so nothing has to
be recovered from a list of keyword arguments later. All of them are optional
and a `nil` member means the specifier was not written, leaving the default to
the runtime.

`newunit` and a user-chosen unit number are alternatives: with `newunit=` the
runtime picks a free unit and stores it in that variable.

## Examples

```clojure
(FileOpen
  :label -1
  :newunit (Var
    :v (SymbolRef 1 "unit")
  )
  :filename (StringConstant
    :s "data.txt"
    :type (String
      :kind 1
      :len (IntegerConstant
        :n 8
        :type (Integer
          :kind 4
        )
        :intboz_type :Decimal
      )
      :len_kind :ExpressionLength
      :physical_type :DescriptorString
    )
  )
  :status (StringConstant
    :s "old"
    :type (String
      :kind 1
      :len (IntegerConstant
        :n 3
        :type (Integer
          :kind 4
        )
        :intboz_type :Decimal
      )
      :len_kind :ExpressionLength
      :physical_type :DescriptorString
    )
  )
  :form nil
  :access nil
  :iostat (Var
    :v (SymbolRef 1 "iostat")
  )
  :iomsg nil
  :action nil
  :delim nil
  :recl nil
  :position nil
  :blank nil
  :encoding nil
  :sign nil
  :decimal nil
  :round nil
  :pad nil
  :asynchronous nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/file_stmt.asr
:language: clojure
```

## See Also

[FileClose](FileClose.md), [FileRead](FileRead.md), [FileWrite](FileWrite.md), [FileInquire](FileInquire.md)
