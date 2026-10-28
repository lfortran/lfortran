! With --implicit-argument-casting, an external procedure defined in the same
! file is called with an actual of another type than its dummy: a REAL array
! to a COMPLEX dummy, and an allocatable COMPLEX array to a REAL dummy.
program implicit_argument_casting_05
implicit none
real(8) :: x(4) = [1d0, 2d0, 3d0, 4d0]
complex(8), allocatable :: z(:)

call times_i_05(x, 2)
print '(4F6.1)', x
if (any(abs(x - [-2d0, 1d0, -4d0, 3d0]) > 1d-12)) error stop 1

allocate(z(2))
z = [(1d0, 2d0), (3d0, 4d0)]
call double_05(z, 4)
print '(4F6.1)', z
if (abs(z(1) - (2d0, 4d0)) > 1d-12) error stop 2
if (abs(z(2) - (6d0, 8d0)) > 1d-12) error stop 3
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
