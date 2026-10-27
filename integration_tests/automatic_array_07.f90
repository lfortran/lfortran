! The bounds and extents of explicit-shape arrays are fixed on entry also
! for a VALUE bound, a BLOCK entered again in a loop, a host array used by
! an internal procedure, and INTENT(OUT) dummies that are finalized or
! have their allocatable components deallocated on entry.
module automatic_array_07_m
implicit none
type :: t
    real, allocatable :: v(:)
end type
type :: u
    integer :: i = 0
contains
    final :: fin
end type
integer :: nfin = 0
contains

subroutine fin(x)
    type(u), intent(inout) :: x(:)
    nfin = nfin + size(x)
end subroutine

subroutine value_bound(n, a, expected)
    integer, value :: n
    real :: a(n)
    integer, intent(in) :: expected
    n = 99
    if (size(a) /= expected) error stop 1
    if (ubound(a, 1) /= expected) error stop 2
end subroutine

subroutine block_in_loop(m)
    integer, intent(in) :: m
    integer :: i, n
    do i = 1, 3
        n = m - i
        block
            real :: a(n), b(2:n)
            a = 1
            n = 99
            if (size(a) /= max(0, m - i)) error stop 3
            if (sum(a) /= max(0, m - i)) error stop 4
            if (size(b) /= max(0, m - i - 1)) error stop 5
            if (size(b) > 0) then
                if (lbound(b, 1) /= 2 .or. ubound(b, 1) /= m - i) error stop 6
            else
                if (lbound(b, 1) /= 1 .or. ubound(b, 1) /= 0) error stop 7
            end if
        end block
    end do
end subroutine

subroutine host(n, expected)
    integer :: n
    integer, intent(in) :: expected
    real :: a(n)
    a = 7
    n = 50
    call inner()
contains
    subroutine inner()
        if (size(a) /= expected) error stop 8
        if (ubound(a, 1) /= expected) error stop 9
        if (sum(a) /= 7 * expected) error stop 10
    end subroutine
end subroutine

subroutine intent_out(lo, n, a, b)
    integer, intent(in) :: lo, n
    type(t), intent(out) :: a(lo:n)
    type(u), intent(out) :: b(n)
    if (size(a) /= max(0, n - lo + 1)) error stop 11
    if (size(b) /= max(0, n)) error stop 12
    if (size(a) == 0) then
        if (lbound(a, 1) /= 1 .or. ubound(a, 1) /= 0) error stop 13
    end if
end subroutine

end module

program automatic_array_07
use automatic_array_07_m
implicit none
real :: x(3) = [1., 2., 3.]
type(t) :: y(3)
type(u) :: z(3)
integer :: i, k

call value_bound(-1, x, 0)
call value_bound(3, x, 3)

call block_in_loop(3)

k = 2
call host(k, 2)
k = -3
call host(k, 0)

do i = 1, 3
    allocate(y(i)%v(2))
end do
call intent_out(2, 0, y, z)
if (.not. allocated(y(1)%v)) error stop 14
if (nfin /= 0) error stop 15
call intent_out(1, 2, y, z)
if (allocated(y(1)%v) .or. allocated(y(2)%v)) error stop 16
if (.not. allocated(y(3)%v)) error stop 17
if (nfin /= 2) error stop 18
print *, "ok"
end program
