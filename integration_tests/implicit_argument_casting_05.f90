! With --implicit-argument-casting, an external procedure defined in the same
! file is called with an actual of another type than its dummy: a REAL array
! to a COMPLEX dummy, an allocatable COMPLEX array to a REAL dummy, an
! INTEGER scalar to a REAL dummy and a REAL(8) scalar to a REAL(4) dummy,
! from the main program and from a module procedure. The dummy names the
! storage of the actual.
module implicit_argument_casting_05_mod
implicit none
contains
    subroutine rotate(x, m)
    integer, intent(in) :: m
    real(8), intent(inout) :: x(m)
    call times_i_05(x, m/2)
    end subroutine
end module

program implicit_argument_casting_05
use implicit_argument_casting_05_mod
implicit none
real(8) :: x(4) = [1d0, 2d0, 3d0, 4d0]
complex(8), allocatable :: z(:)
integer :: i
real(8) :: d
real :: y

call times_i_05(x, 2)
print '(4F6.1)', x
if (any(abs(x - [-2d0, 1d0, -4d0, 3d0]) > 1d-12)) error stop 1

allocate(z(2))
z = [(1d0, 2d0), (3d0, 4d0)]
call double_05(z, 4)
print '(4F6.1)', z
if (abs(z(1) - (2d0, 4d0)) > 1d-12) error stop 2
if (abs(z(2) - (6d0, 8d0)) > 1d-12) error stop 3

x = [1d0, 2d0, 3d0, 4d0]
call rotate(x, 4)
print '(4F6.1)', x
if (any(abs(x - [-2d0, 1d0, -4d0, 3d0]) > 1d-12)) error stop 4

i = 0
call set_one_05(i)
print *, i
if (i /= transfer(1.0, 0)) error stop 5

d = transfer([1.5, 2.5], 0d0)
call first_half_05(d, y)
print *, y
if (y /= 1.5) error stop 6
end program

subroutine times_i_05(c, n)
implicit none
integer :: n
complex(8) :: c(n)
c = c * (0d0, 1d0)
end subroutine

subroutine double_05(r, n)
implicit none
integer :: n
real(8) :: r(n)
r = 2 * r
end subroutine

subroutine set_one_05(r)
implicit none
real :: r
r = 1
end subroutine

subroutine first_half_05(x, y)
implicit none
real :: x, y
y = x
end subroutine
