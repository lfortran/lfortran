! An internal procedure passes a host allocatable to a procedure that forwards
! it to a new instance of the recursive host: a sibling internal procedure or
! a module procedure (#16010)
module host_assoc_02_m
implicit none
integer :: nseen = 0
contains

recursive subroutine host_sibling(x, depth)
    integer, allocatable, intent(inout) :: x(:)
    integer, intent(in) :: depth
    integer, allocatable :: y(:)
    call inner()
contains
    subroutine inner()
        if (.not. allocated(x)) error stop 3
        if (depth > 1) then
            allocate(y(2))
            y = depth
            call fwd(y)
            if (.not. allocated(y)) error stop 1
            if (any(y /= depth)) error stop 2
            nseen = nseen + 1
        end if
    end subroutine inner
    recursive subroutine fwd(z)
        integer, allocatable, intent(inout) :: z(:)
        call host_sibling(z, depth - 1)
    end subroutine fwd
end subroutine host_sibling

recursive subroutine host_module(x, depth)
    integer, allocatable, intent(inout) :: x(:)
    integer, intent(in) :: depth
    integer, allocatable :: y(:)
    call inner()
contains
    subroutine inner()
        if (.not. allocated(x)) error stop 6
        if (depth > 1) then
            allocate(y(2))
            y = depth
            call fwd(y, depth - 1)
            if (.not. allocated(y)) error stop 4
            if (any(y /= depth)) error stop 5
            nseen = nseen + 1
        end if
    end subroutine inner
end subroutine host_module

recursive subroutine fwd(z, d)
    integer, allocatable, intent(inout) :: z(:)
    integer, intent(in) :: d
    call host_module(z, d)
end subroutine fwd

end module host_assoc_02_m

program host_assoc_02
use host_assoc_02_m
implicit none
integer, allocatable :: a(:)
allocate(a(1))
a = 7
call host_sibling(a, 2)
if (.not. allocated(a)) error stop 7
if (any(a /= 7)) error stop 8
call host_module(a, 3)
if (.not. allocated(a)) error stop 9
if (any(a /= 7)) error stop 10
if (nseen /= 3) error stop 11
print *, "ok"
end program host_assoc_02
