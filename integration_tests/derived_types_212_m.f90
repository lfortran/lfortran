! A bind(c) module array of a derived type with component defaults, used
! from another file (derived_types_212.f90).
module derived_types_212_m
use iso_c_binding, only: c_int, c_double
implicit none
type, bind(c) :: t
    integer(c_int) :: z = 7
    real(c_double) :: r = 1.5d0
end type
type(t), bind(c) :: dt212_arr(4)
type(t), bind(c, name="dt212_named_arr") :: dt212_arr2(3)
contains
subroutine bump()
    dt212_arr(2)%z = dt212_arr(2)%z + 1
end subroutine
end module
