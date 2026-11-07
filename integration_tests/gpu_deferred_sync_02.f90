! A call of a subroutine whose first statement is `if (c) return` tests `c`
! first. When `c` does not hold, the subroutine still sees what the launch
! before the call wrote; a subroutine with an intent(out) dummy, and a call
! whose argument calls a function, still run as written.
module gpu_deferred_sync_02_mod
implicit none
integer :: calls = 0
contains
integer function next()
    calls = calls + 1
    next = calls
end function

subroutine steps(x, directory, seen, last, kept)
    real, intent(inout) :: x(:)
    character(*), intent(in) :: directory
    real, intent(out) :: seen
    integer, intent(out) :: last
    real, allocatable, intent(inout) :: kept(:)
    integer :: i
    seen = -1
    last = 0
    do concurrent (i = 1:size(x))
        x(i) = x(i) + 1
    end do
    call dm(x)
    call dm(2*x(1:2))
    do concurrent (i = 1:size(x))
        x(i) = x(i) + 1
    end do
    call reset(kept)
    call note(next())
contains
    subroutine dm(values)
        real, intent(in) :: values(:)
        if (len_trim(directory) == 0) return
        seen = seen + values(1)
    end subroutine
    subroutine reset(values)
        real, allocatable, intent(out) :: values(:)
        if (len_trim(directory) == 0) return
        allocate(values(3))
        values = 7
    end subroutine
    subroutine note(k)
        integer, intent(in) :: k
        if (len_trim(directory) == 0) return
        last = k
    end subroutine
end subroutine
end module

program gpu_deferred_sync_02
use gpu_deferred_sync_02_mod
implicit none
real :: x(1000), seen
real, allocatable :: kept(:)
integer :: last
x = 1
allocate(kept(5))
call steps(x, '', seen, last, kept)
if (abs(seen + 1) > 0) error stop 1
if (last /= 0) error stop 2
if (allocated(kept)) error stop 3
if (calls /= 1) error stop 4
if (any(abs(x - 3) > 0)) error stop 5
call steps(x, 'dump', seen, last, kept)
! x(1) is 4 when dm runs: seen = -1 + 4 + 8
if (abs(seen - 11) > 0) error stop 6
if (last /= 2) error stop 7
if (.not. allocated(kept)) error stop 8
if (any(abs(kept - 7) > 0)) error stop 9
if (calls /= 2) error stop 10
if (any(abs(x - 5) > 0)) error stop 11
print *, seen, last, calls
end program
