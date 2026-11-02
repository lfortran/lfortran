! The result of an internal function referenced through a dummy procedure
! is finalized as often as when the internal function is referenced
! directly: the procedure value hands the result to its caller without
! finalizing it, or its own result variable, once more.
module finalization_49_mod
implicit none
integer :: finalized = 0
type :: t
    integer :: v = 0
contains
    final :: finalize
end type
contains
subroutine finalize(x)
    type(t), intent(inout) :: x
    finalized = finalized + 1
end subroutine
integer function via(f)
    interface
        function f() result(r)
            import :: t
            type(t) :: r
        end function
    end interface
    type(t) :: x
    x = f()
    via = x%v
end function
integer function via_allocatable(f)
    interface
        function f() result(r)
            import :: t
            type(t), allocatable :: r
        end function
    end interface
    type(t), allocatable :: x
    x = f()
    via_allocatable = x%v
end function
subroutine host(n)
    integer, intent(in) :: n
    integer :: v
    type(t) :: x
    type(t), allocatable :: y
    ! x and the result of make
    finalized = 0
    x = make()
    if (finalized /= 2) error stop
    ! the local x of via, as above, and when via returns
    finalized = 0
    v = via(make)
    if (v /= n) error stop
    if (finalized /= 3) error stop
    ! the result of make_allocatable; y is not allocated
    finalized = 0
    y = make_allocatable()
    if (finalized /= 1) error stop
    ! as above, and the local y of via_allocatable when it returns
    finalized = 0
    v = via_allocatable(make_allocatable)
    if (v /= n + 1) error stop
    if (finalized /= 2) error stop
contains
    function make() result(r)
        type(t) :: r
        r%v = n
    end function
    function make_allocatable() result(r)
        type(t), allocatable :: r
        allocate(r)
        r%v = n + 1
    end function
end subroutine
end module
program finalization_49
use finalization_49_mod
implicit none
call host(3)
print *, "ok"
end program
