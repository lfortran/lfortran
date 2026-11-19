! In a procedure with internal procedures, a call of a procedure that is not
! pure keeps a local known to be unallocated unless an internal procedure
! names the local: only an internal procedure can change a local of its host
! that is not an actual argument, also when it is passed to the procedure
! called. The first assignment to such a local allocates it without a test.
module realloc_lhs_unallocated_03_m
implicit none
integer :: calls = 0
contains
subroutine tick()
    calls = calls + 1
end subroutine
subroutine run(p)
    interface
        subroutine p()
        end subroutine
    end interface
    call p()
end subroutine
end module

program realloc_lhs_unallocated_03
use realloc_lhs_unallocated_03_m
implicit none
call untouched()
call allocated_by_internal()
call allocated_through_argument()
if (calls /= 3) error stop 10
print *, "ok"

contains

! The internal procedure names only `n`: `t` is still unallocated after the
! calls
subroutine untouched()
    real, allocatable :: t(:)
    integer :: n
    n = 0
    call tick()
    call count_up()
    t = [1., 2., 3.]
    if (size(t) /= 3 .or. lbound(t, 1) /= 1 .or. any(t /= [1., 2., 3.])) error stop 1
    if (n /= 1) error stop 2
contains
    subroutine count_up()
        n = n + 1
    end subroutine
end subroutine

! The internal procedure allocates `t`
subroutine allocated_by_internal()
    real, allocatable :: t(:)
    call tick()
    call make()
    if (size(t) /= 5) error stop 3
    t = [1., 2.]
    if (size(t) /= 2 .or. any(t /= [1., 2.])) error stop 4
contains
    subroutine make()
        allocate(t(0:4))
        t = 0
    end subroutine
end subroutine

! The internal procedure that allocates `t` is called by another procedure
subroutine allocated_through_argument()
    real, allocatable :: t(:)
    call tick()
    call run(make)
    if (.not. allocated(t)) error stop 5
    t = [7., 8., 9.]
    if (size(t) /= 3 .or. lbound(t, 1) /= 1 .or. any(t /= [7., 8., 9.])) error stop 6
contains
    subroutine make()
        allocate(t(2))
        t = 1
    end subroutine
end subroutine

end program
