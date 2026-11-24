module struct_member_order_fortran_13789_m
implicit none
type, bind(c) :: crec
    real(8) :: z
    integer(4) :: a
end type crec
type :: rec
    real(4) :: z
    integer(4) :: a
end type rec
end module struct_member_order_fortran_13789_m

program struct_member_order_fortran_13789
use struct_member_order_fortran_13789_m, only: crec
use struct_member_order_fortran_13789_m, only: rec
implicit none
type(crec) :: c
type(rec) :: r
r = rec(1.50000000e+00, 7)
c = crec(2.5000000000000000e+00_8, 9)
if (r%a /= 7 .or. c%a /= 9) then
    error stop
end if
end program struct_member_order_fortran_13789
