module finalization_42_m
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
    end subroutine

end module

program finalization_42
    use finalization_42_m
    implicit none
    type(item) :: y
    type(item), allocatable :: t
    class(item), allocatable :: c, z
    class(base), allocatable :: b, s

    ! F2018 7.5.6.3 p1: the variable of an intrinsic assignment is finalized
    ! before it is defined, unless it is an unallocated allocatable variable.
    y%v = 1
    t = y
    if (finalized /= 0) error stop 1
    if (t%v /= 1) error stop 2
    y%v = 2
    t = y
    if (finalized /= 1) error stop 3
    if (last_value /= 1) error stop 4
    if (t%v /= 2) error stop 5

    ! A polymorphic variable is reallocated with the dynamic type of the
    ! expression: only its previous value is finalized.
    finalized = 0
    last_value = 0
    c = y
    if (finalized /= 0) error stop 6
    if (c%v /= 2) error stop 7
    y%v = 3
    c = y
    if (finalized /= 1) error stop 8
    if (last_value /= 2) error stop 9
    if (c%v /= 3) error stop 10
    allocate(z)
    z%v = 4
    finalized = 0
    c = z
    if (finalized /= 1) error stop 11
    if (last_value /= 3) error stop 12
    if (c%v /= 4) error stop 13

    ! A polymorphic expression assigned to a nonpolymorphic variable.
    finalized = 0
    t = z
    if (finalized /= 1) error stop 14
    if (last_value /= 2) error stop 15
    if (t%v /= 4) error stop 16

    ! A sourced allocation finalizes nothing.
    finalized = 0
    deallocate(c)
    if (finalized /= 1) error stop 17
    finalized = 0
    allocate(c, source=z)
    if (finalized /= 0) error stop 18
    if (c%v /= 4) error stop 19
    allocate(item :: s)
    s%v = 5
    allocate(b, source=s)
    if (finalized /= 0) error stop 20
    if (b%v /= 5) error stop 21
    deallocate(b)
    if (finalized /= 1) error stop 22
    if (last_value /= 5) error stop 23

    print *, "ok"
end program
