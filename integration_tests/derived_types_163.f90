! A derived-type parameter imported with `use` (or by submodule host
! association) used as the initializer of a derived-type entity.
module derived_types_163_m
implicit none
type :: t
    integer :: i = 0
end type
type(t), parameter :: z = t(7)
type(t), target, save :: tgt = t(5)
interface
    module integer function g()
    end function
end interface
end module

module derived_types_163_m2
use derived_types_163_m, only: t, z
implicit none
type :: b_t
    type(t) :: p = z
end type
type(t) :: mv = z
end module

submodule (derived_types_163_m) derived_types_163_sm
implicit none
type(t) :: smv = z
contains
module integer function g()
    g = smv%i
end function
end submodule

program derived_types_163
use derived_types_163_m, only: t, z, tgt, g
use derived_types_163_m2, only: b_t, mv
implicit none
type(t) :: x = z
type(t), parameter :: y = z
type(t), pointer :: p => tgt
type(b_t) :: b
if (x%i /= 7) error stop
if (y%i /= 7) error stop
if (b%p%i /= 7) error stop
if (mv%i /= 7) error stop
if (p%i /= 5) error stop
if (g() /= 7) error stop
print *, x%i, y%i, b%p%i, mv%i, p%i, g()
end program
