! The AST keeps declarations and statements in a single list, in source order,
! so `--show-ast-f90` reproduces the order below instead of hoisting every
! declaration above the `format` and `data` statements.
program decl_order_02
use iso_fortran_env, only: int32
100 format ("total = ", i0)
implicit none
integer(int32) :: a
data a/2/
integer(int32) :: b
data b/40/
integer(int32) :: total
total = a + b
write (*, 100) total
if (total /= 42) error stop
end program
