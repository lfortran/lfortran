module select_type_60_m
    implicit none
    integer :: finalized = 0
    type, abstract :: base
    end type
    type, extends(base) :: item
        integer :: v = 0
    contains
        final :: finish
    end type
contains
    subroutine finish(x)
        type(item) :: x
        finalized = finalized + 1
    end subroutine

    subroutine accept(x)
        type(item), intent(in) :: x
        if (x%v /= 7) error stop 10
    end subroutine

    subroutine accept_class(x)
        class(item), intent(in) :: x
        if (x%v /= 7) error stop 11
    end subroutine

    subroutine modify(x)
        type(item) :: x
        x%v = x%v + 1
    end subroutine
end module

program select_type_60
    use select_type_60_m
    implicit none
    class(base), allocatable :: x

    allocate(item :: x)
    select type (x)
    type is (item)
        x%v = 7
        call accept(x)
        call accept_class(x)
    end select
    print *, finalized
    if (finalized /= 0) error stop 1

    select type (x)
    type is (item)
        call modify(x)
    end select
    if (finalized /= 0) error stop 2
    select type (x)
    type is (item)
        if (x%v /= 8) error stop 3
    end select

    deallocate(x)
    if (finalized /= 1) error stop 4
end program
