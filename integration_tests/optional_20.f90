! An absent optional intent(out) scalar dummy is neither finalized,
! deallocated nor default-initialized on entry (#13435, #14077).
module optional_20_mod
implicit none
integer :: nfin = 0
type :: tf
    integer :: n = 5
contains
    final :: fin
end type
type :: tc
    type(tf) :: f
    integer :: m = 6
end type
type :: ta
    integer, allocatable :: q(:)
    integer :: z = 3
end type
contains
subroutine fin(self)
    type(tf), intent(inout) :: self
    nfin = nfin + 1
    self%n = -9
end subroutine
subroutine s(x, y, z, w)
    type(tf), intent(out), optional :: x
    type(tc), intent(out), optional :: y
    type(ta), intent(out), optional :: z
    integer, allocatable, intent(out), optional :: w(:)
    if (present(x)) then
        if (x%n /= 5) error stop "x not initialized"
        x%n = 1
    end if
    if (present(y)) then
        if (y%f%n /= 5 .or. y%m /= 6) error stop "y not initialized"
        y%m = 2
    end if
    if (present(z)) then
        if (allocated(z%q) .or. z%z /= 3) error stop "z not initialized"
        z%z = 4
    end if
    if (present(w)) then
        if (allocated(w)) error stop "w not deallocated"
        allocate(w(1))
        w = 8
    end if
end subroutine
end module

program optional_20
use optional_20_mod
implicit none
type(tf) :: x
type(tc) :: y
type(ta) :: z
integer, allocatable :: w(:)

nfin = 0
call s()
if (nfin /= 0) error stop 1

x%n = -1
nfin = 0
call s(x=x)
if (nfin /= 1) error stop 2
if (x%n /= 1) error stop 3

y%m = -1
nfin = 0
call s(y=y)
if (nfin /= 1) error stop 4
if (y%m /= 2 .or. y%f%n /= 5) error stop 5

allocate(z%q(3))
z%z = -1
nfin = 0
call s(z=z)
if (nfin /= 0) error stop 6
if (allocated(z%q) .or. z%z /= 4) error stop 7

allocate(w(5))
call s(w=w)
if (size(w) /= 1 .or. w(1) /= 8) error stop 8
print *, "ok"
end program
