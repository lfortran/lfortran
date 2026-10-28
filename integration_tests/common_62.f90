! A whole component operand read in place must not share storage with the
! assignment target through COMMON (#16215)
module common_62_m
implicit none
type t
    sequence
    real :: x(4)
    real :: y(4)
end type
end module

module common_62_n
implicit none
real :: w, z(7)
common /c62b/ w, z
end module

program common_62
use common_62_m, only: t
implicit none
type(t) :: a, b
common /c62a/ a
common /c62b/ b

a%x = [1., 10., 100., 1000.]
a%y = 10
call host()
print *, a%x, a%y(1)
if (any(a%x /= [1., 2., 11., 101.])) error stop 1
if (a%y(1) /= 1001.) error stop 2

a%x = [1., 10., 100., 1000.]
a%y = 10
call host_unary()
print *, a%x, a%y(1)
if (any(a%x /= [1., -1., -10., -100.])) error stop 3
if (a%y(1) /= -1000.) error stop 6

b%x = [1., 10., 100., 1000.]
b%y = 10
call use_module()
print *, b%x, b%y(1)
if (any(b%x /= [1., 2., 11., 101.])) error stop 4
if (b%y(1) /= 1001.) error stop 5

contains

! z(k) is the host's a%x(k + 1)
subroutine host()
    real :: w, z(7)
    common /c62a/ w, z
    z(1:4) = a%x + 1
end subroutine

subroutine host_unary()
    real :: w, z(7)
    common /c62a/ w, z
    z(1:4) = -a%x
end subroutine

! The module's z(k) is b%x(k + 1)
subroutine use_module()
    use common_62_n, only: z
    z(1:4) = b%x + 1
end subroutine
end program
