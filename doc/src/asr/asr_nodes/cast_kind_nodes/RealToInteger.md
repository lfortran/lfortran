# RealToInteger

Read To Integer cast kind type, a **cast_kind** node.

## Declaration

### Syntax

```fortran
ASR::cast_kindType::RealToInteger
```

### Arguments

None.

### Return values

Defines the cast kind type of the type.

## Description

**RealToInteger** is used to define the cast kind type of the variable. It is
used while typecasting or type conversion.

## Utility Functions/Method and Types

```c
int cast_kind = -1;
case ASR::ttypeType::Real: {
	cast_kind = ASR::cast_kindType::RealToInteger
}
```

is used to assign to define the realtointeger cast kind. It is used with
following scope types:

```c
LFortran::ASRUtils::expr_type(x)->type
ASR::expr_t* x, ASR::ttype_t* int64type
```
## Types

RealToInteger.

```c
Allocate(alloc_arg* args, expr? stat, expr? errmsg, expr? source)
```
## Examples

It is mostly used in during code generation of ASR utils to C, C++, Julia, Wasm
and LLVM.

## See Also

[RealToReal](RealToReal.md), [RealToComplex](RealToComplex.md), [RealToLogical](RealToLogical.md),
[RealToCharacter](RealToCharacter.md)
