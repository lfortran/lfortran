# FileInquire

Asks the runtime about a unit or a file.

## Declaration

### Syntax

```text
FileInquire(int label, expr? unit, expr? file, expr? iostat, expr? err,
    expr? exist, expr? opened, expr? number, expr? named, expr? name,
    expr? access, expr? sequential, expr? direct, expr? form,
    expr? formatted, expr? unformatted, expr? recl, expr? nextrec,
    expr? blank, expr? position, expr? action, expr? read, expr? write,
    expr? readwrite, expr? delim, expr? pad, expr? flen,
    expr? blocksize, expr? convert, expr? carriagecontrol, expr? size,
    expr? pos, expr? iolength, expr* iolength_vars, expr? decimal,
    expr? sign, expr? encoding, expr? stream, expr? iomsg, expr? round,
    expr? pending, expr? asynchronous)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `label` | the statement label of the Fortran statement, or `-1` when it has none. |
| `unit` | the unit to ask about. Exactly one of `unit` and `file` is given. |
| `file` | the file name to ask about. |
| `iostat` | a variable receiving the I/O status: zero on success, non-zero otherwise. The statement does not abort when it is present. |
| `err` | the label to branch to when the operation fails. |
| `exist` | a logical variable receiving whether the file exists. |
| `opened` | a logical variable receiving whether it is connected. |
| `number` | an integer variable receiving the unit number. |
| `named` | a logical variable receiving whether the unit has a name. |
| `name` | a character variable receiving the file name. |
| `access` | receives `sequential`, `direct` or `stream`. |
| `sequential` | receives whether sequential access is allowed. |
| `direct` | receives whether direct access is allowed. |
| `form` | receives `formatted` or `unformatted`. |
| `formatted` | receives whether formatted I/O is allowed. |
| `unformatted` | receives whether unformatted I/O is allowed. |
| `recl` | receives the record length. |
| `nextrec` | receives the number of the next record. |
| `blank` | receives the blank mode. |
| `position` | receives the position mode. |
| `action` | receives `read`, `write` or `readwrite`. |
| `read` | receives whether reading is allowed. |
| `write` | receives whether writing is allowed. |
| `readwrite` | receives whether both are allowed. |
| `delim` | receives the delimiter mode. |
| `pad` | receives the pad mode. |
| `flen` | receives the length of the file. |
| `blocksize` | receives the block size of the connection. |
| `convert` | receives the byte order conversion mode. |
| `carriagecontrol` | receives the carriage control mode. |
| `size` | receives the size of the file in bytes. |
| `pos` | receives the current stream position. |
| `iolength` | receives the record length needed for `iolength_vars`. |
| `iolength_vars` | the output list to measure for `iolength=`. |
| `decimal` | receives the decimal edit mode. |
| `sign` | receives the sign mode. |
| `encoding` | receives the encoding. |
| `stream` | receives whether stream access is allowed. |
| `iomsg` | a variable receiving the error message when the operation fails. |
| `round` | receives the rounding mode. |
| `pending` | receives whether an asynchronous transfer is pending. |
| `asynchronous` | receives whether asynchronous transfer is allowed. |

### Return values

None.

## Description

`inquire` is one statement with many independent outputs, and each of them is
a member here. Every member other than the thing being asked about is a
variable the runtime writes to, so a backend implements the statement by
filling in the members that are not `nil`.

The `iolength=` form is different from the others: it measures the output list
in `iolength_vars` instead of asking about a connection, and neither `unit`
nor `file` is given.

## Examples

```clojure
(FileInquire
  :label -1
  :unit (Var
    :v (SymbolRef 1 "unit")
  )
  :file nil
  :iostat nil
  :err nil
  :exist (Var
    :v (SymbolRef 1 "exists")
  )
  :opened nil
  :number nil
  :named nil
  :name nil
  :access nil
  :sequential nil
  :direct nil
  :form nil
  :formatted nil
  :unformatted nil
  :recl nil
  :nextrec nil
  :blank nil
  :position nil
  :action nil
  :read nil
  :write nil
  :readwrite nil
  :delim nil
  :pad nil
  :flen nil
  :blocksize nil
  :convert nil
  :carriagecontrol nil
  :size nil
  :pos nil
  :iolength nil
  :iolength_vars []
  :decimal nil
  :sign nil
  :encoding nil
  :stream nil
  :iomsg nil
  :round nil
  :pending nil
  :asynchronous nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/file_stmt.asr
:language: clojure
```

## See Also

[FileOpen](FileOpen.md), [FileClose](FileClose.md)
