# GpuKernelFunction

A procedure that runs on a GPU.

## Declaration

### Syntax

```text
GpuKernelFunction(symbol_table symtab, identifier name,
    ttype function_signature, identifier* dependencies, expr* args,
    stmt* body, access access, location start_name, location end_name)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `symtab` | the symbol table of the kernel: its arguments and its local variables. |
| `name` | the name of the kernel. |
| `function_signature` | a [FunctionType](../type_nodes/FunctionType.md) with no `return_var_type`: a kernel returns nothing. |
| `dependencies` | the names of the symbols the body refers to. |
| `args` | the arguments of the kernel, as `Var` expressions. |
| `body` | the statements executed by every thread of the kernel. |
| `access` | `Public` or `Private`. |
| `start_name` | the source span of the name in the opening statement. |
| `end_name` | the source span of the name in the closing statement. |

### Return values

None.

## Description

A **GpuKernelFunction** is separate from [Function](Function.md) because it is
not callable in the ordinary sense: it is launched, with an execution
configuration, by
[GpuKernelLaunch](../statement_nodes/GpuKernelLaunch.md), and it always
returns nothing.

Inside the body, the position of the running thread is read with
[GpuThreadIndex](../expression_nodes/GpuThreadIndex.md),
[GpuBlockIndex](../expression_nodes/GpuBlockIndex.md) and
[GpuBlockSize](../expression_nodes/GpuBlockSize.md).

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/gpukernelfunction.asr
:language: clojure
```

## See Also

[GpuKernelLaunch](../statement_nodes/GpuKernelLaunch.md), [GpuThreadIndex](../expression_nodes/GpuThreadIndex.md), [GpuBlockIndex](../expression_nodes/GpuBlockIndex.md), [Function](Function.md)
