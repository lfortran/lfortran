! An internal procedure sees a host allocatable of the main program, and an
! allocatable dummy of a procedure, with its lower bounds (#13043)
module host_assoc_03_m
implicit none
contains
subroutine shift(v)
    real, allocatable, intent(inout) :: v(:, :)
    call check()
    if (lbound(v, 1) /= 0 .or. ubound(v, 2) /= 7) error stop 1
contains
    subroutine check()
        if (lbound(v, 1) /= -1 .or. ubound(v, 1) /= 1) error stop 2
        if (lbound(v, 2) /= 3 .or. ubound(v, 2) /= 4) error stop 3
        if (v(-1, 3) /= 5.0) error stop 4
        deallocate(v)
        allocate(v(0:2, 5:7))
    end subroutine check
end subroutine shift
end module host_assoc_03_m

program host_assoc_03
use host_assoc_03_m
implicit none
integer, allocatable :: w(:)
real, allocatable :: v(:, :)
allocate(w(-2:2))
w = [1, 2, 3, 4, 5]
call show()
if (w(0) /= 30) error stop 9
allocate(v(-1:1, 3:4))
v = 5.0
call shift(v)
if (lbound(v, 1) /= 0 .or. lbound(v, 2) /= 5) error stop 5
print *, "ok"
contains
subroutine show()
    if (lbound(w, 1) /= -2) error stop 6
    if (ubound(w, 1) /= 2) error stop 7
    if (w(-2) /= 1 .or. w(2) /= 5) error stop 8
    w(0) = 30
end subroutine show
end program host_assoc_03
