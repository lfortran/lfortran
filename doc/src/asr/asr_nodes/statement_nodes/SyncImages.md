# SyncImages

Waits until the named images reach this point.

## Declaration

### Syntax

```text
SyncImages(expr? image_set, expr? stat, expr? errmsg)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `image_set` | the image, or array of images, to synchronise with. `nil` means all images. |
| `stat` | a variable receiving the status. |
| `errmsg` | a character variable receiving the error message. |

### Return values

None.

## Description

`sync images` synchronises with a subset of the images rather than with all of
them, which lets a producer wait only for its consumer. The synchronisation
must be symmetric: an image named here must itself name this image.

## Examples

```clojure
(SyncImages
  :image_set (Var
    :v (SymbolRef 1 "n")
  )
  :stat (Var
    :v (SymbolRef 1 "stat")
  )
  :errmsg nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/coarray_stmt.asr
:language: clojure
```

## See Also

[SyncAll](SyncAll.md), [SyncMemory](SyncMemory.md), [CoarrayRef](../expression_nodes/CoarrayRef.md)
