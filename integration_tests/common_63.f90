! The same component of two different variables is not told apart when the
! variables share storage through COMMON (#16215)
module common_63_m
implicit none
type t
    sequence
    real :: x(4)
    real :: y(4)
end type
end module

module common_63_n
use common_63_m, only: t
implicit none
real :: w
type(t) :: c
common /c63b/ w, c
end module

program common_63
use common_63_m, only: t
implicit none
type(t) :: a, b
! A named common block has the same size everywhere (F2018 8.10.2.5).
real :: pada, padb
common /c63a/ a, pada
common /c63b/ b, padb

a%x = [1., 10., 100., 1000.]
a%y = 10
call host()
print *, a%x
if (any(a%x /= [1., 2., 11., 101.])) error stop 1

a%x = [1., 10., 100., 1000.]
call host_copy()
print *, a%x
if (any(a%x /= [1., 1., 10., 100.])) error stop 2

b%x = [1., 10., 100., 1000.]
b%y = 10
call use_module()
print *, b%x
if (any(b%x /= [1., 2., 11., 101.])) error stop 3

contains

! c%x(k) is the host's a%x(k + 1)
subroutine host()
    real :: w
    type(t) :: c
    common /c63a/ w, c
    c%x(1:3) = a%x(1:3) + 1
end subroutine

subroutine host_copy()
    real :: w
    type(t) :: c
    common /c63a/ w, c
    c%x(1:3) = a%x(1:3)
end subroutine

! The module's c%x(k) is b%x(k + 1)
subroutine use_module()
    use common_63_n, only: c
    c%x(1:3) = b%x(1:3) + 1
end subroutine
end program
