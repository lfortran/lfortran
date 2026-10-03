program precision_real_01
implicit none
integer, parameter :: sp = kind(1.0)
integer, parameter :: dp = kind(1.0d0)
real(dp) :: x, a(3)
real(sp) :: y
real(dp), allocatable :: b(:)

! Single precision literal assigned to double precision: warn
x = 1.3
x = 0.1
x = 1.3e-2
a = 2.7
allocate(b(2))
b = 3.3
a(2) = 1.1

! Double precision literal: no warning
x = 1.3_dp
x = 1.3_8
x = 1.3d0

! Single precision made explicit with a kind designator: no warning
x = 1.3_sp
x = 1.3_4

! Exactly representable in single precision: no warning
x = 1.0
x = 0.5
x = 2.
x = 1.5e2

! The variable is single precision: no warning
y = 1.3
y = 1.3_dp
y = 1.3_8

! Not a real literal: no warning
x = 1
x = y

call no_kind_parameter()
call imported_kind_parameter()
print *, x, y, a, b
end program

! No named kind constant is in scope: the hint uses the kind value
subroutine no_kind_parameter()
implicit none
double precision :: z
z = 1.3
print *, z
end subroutine

! The named kind constant comes from a module
subroutine imported_kind_parameter()
use iso_fortran_env, only: real64
implicit none
real(real64) :: w
w = 1.3
print *, w
end subroutine
