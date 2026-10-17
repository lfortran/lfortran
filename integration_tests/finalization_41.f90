module finalization_41_m
    implicit none
    integer :: finalized = 0
    integer :: last_value = 0

    type :: base
        integer :: v = 0
    end type

    type, extends(base) :: item
    contains
        final :: finish
    end type

contains

    subroutine finish(x)
        type(item), intent(inout) :: x
        finalized = finalized + 1
        last_value = x%v
        x%v = -1
    end subroutine

    function make_class(v) result(x)
        integer, intent(in) :: v
        class(item), allocatable :: x
        allocate(x)
        x%v = v
    end function

    function make_type(v) result(x)
        integer, intent(in) :: v
        type(item), allocatable :: x
        allocate(x)
        x%v = v
    end function

    function make_base(v) result(x)
        integer, intent(in) :: v
        class(base), allocatable :: x
        allocate(item :: x)
        x%v = v
    end function

end module

program finalization_41
    use finalization_41_m
    implicit none
    class(item), allocatable :: c
    type(item), allocatable :: t
    class(base), allocatable :: b

    ! A polymorphic allocatable result is finalized after the assignment
    ! (F2018 7.5.6.3 p5); the unallocated variable is not finalized.
    c = make_class(1)
    if (finalized /= 1) error stop 1
    if (last_value /= 1) error stop 2
    if (c%v /= 1) error stop 3

    ! The allocated variable is finalized before it is defined, and the
    ! result after the statement.
    finalized = 0
    c = make_class(2)
    if (finalized /= 2) error stop 4
    if (last_value /= 2) error stop 5
    if (c%v /= 2) error stop 6
    finalized = 0
    deallocate(c)
    if (finalized /= 1) error stop 7
    if (last_value /= 2) error stop 8

    ! A nonpolymorphic allocatable result.
    finalized = 0
    t = make_type(3)
    if (finalized /= 1) error stop 9
    if (last_value /= 3) error stop 10
    if (t%v /= 3) error stop 11
    finalized = 0
    t = make_type(5)
    if (finalized /= 2) error stop 12
    if (last_value /= 5) error stop 13
    if (t%v /= 5) error stop 14
    finalized = 0
    deallocate(t)
    if (finalized /= 1) error stop 15

    ! A polymorphic result assigned to a nonpolymorphic variable.
    finalized = 0
    t = make_class(6)
    if (finalized /= 1) error stop 16
    if (last_value /= 6) error stop 17
    if (t%v /= 6) error stop 18
    finalized = 0
    deallocate(t)
    if (finalized /= 1) error stop 19

    ! The dynamic type of the result has a final subroutine that its
    ! declared type does not.
    finalized = 0
    b = make_base(4)
    if (finalized /= 1) error stop 20
    if (last_value /= 4) error stop 21
    if (b%v /= 4) error stop 22
    finalized = 0
    deallocate(b)
    if (finalized /= 1) error stop 23
    if (last_value /= 4) error stop 24

    print *, "ok"
end program
