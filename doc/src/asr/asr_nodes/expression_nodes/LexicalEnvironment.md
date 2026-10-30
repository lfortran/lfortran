# LexicalEnvironment

The declaration-context frame of an interface specification expression.

## Declaration

### Syntax

```text
LexicalEnvironment(expr frame)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `frame` | the visible variable designating the lexical host frame. |

## Description

Its type is derived from `frame`. This binding belongs to the interface's
declaration, independently of the environment of the callback subsequently
selected as its target. Positional argument substitution must preserve it.
The printer uses internal `lexical_environment(...)` pseudo-Fortran.
