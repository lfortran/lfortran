# IntegerBinOp

Integer Binary Operation expression type. An **expr** node.

## Declaration

### Syntax

```fortran
IntegerBinOp(expr left, binop op, expr right, ttype type, expr? value)
```

### Arguments

`left` and `right` represent expression on the left and right side of operator
`op`. `type` represents table entry type and `value` represents expression.

### Return values

None, as it is an expression type.

## Description

**IntegerBinOp** represents integer binary operation expression type. It is an
ASR expr node. ASR has multiple binary expression types, for example: for Integer,
real, complex, and logical.

If an binary expression is applying binary operator on integer operands,
`IntegerBinOp` is called which then creates LFortran expression `EXPR` using,
`ASR::make_IntegerBinOp_t` method call.

The binary operations accept two arguments of the same type. **IntegerBinOp**
only accepts integers.

## Utility Functions/Method and Types

- `ASR::make_IntegerBinOp_t(...)`
- `Lfortran::ASRUtils::EXPR`

## Types

Only accepts integers.

## Examples

Following example code creates LFortran expression from ASR's `IntegerBinOp`:

```fortran
switch( x.class_type ) {
	case ASR::exprType::IntegerBinOp:
	op_el_wise = LFortran::ASRUtils::EXPR(ASR::make_IntegerBinOp_t(
	                                      al, x.base.base.loc, ref_1,
										  (ASR::binopType)x.m_op, ref_2, x.m_type,
										  nullptr));
```

## See Also

[RealBinOp](), [ComplexBinOp](), [LogicalBinOp]()
