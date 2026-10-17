module select_type_60_m
    implicit none
    integer :: finalized = 0
    type, abstract :: base
    end type
    type, extends(base) :: item
        integer :: v = 0
    contains
        final :: finish
        procedure :: get
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

    subroutine accept_value(x)
        type(item), value :: x
        if (x%v /= 8) error stop 12
        x%v = 100
    end subroutine

    subroutine accept_base(x)
        class(base), intent(in) :: x
        select type (x)
        type is (item)
            if (x%v /= 8) error stop 13
        class default
            error stop 14
        end select
    end subroutine

    integer function f(x)
        type(item), intent(in) :: x
        f = x%v
    end function

    integer function get(self)
        class(item), intent(in) :: self
        get = self%v
    end function
end module

program select_type_60
    use select_type_60_m
    implicit none
    class(base), allocatable :: x
    integer :: n

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

    select type (x)
    type is (item)
        call accept_value(x)
        if (x%v /= 8) error stop 5
    end select
    ! The selector is copied for a VALUE dummy, and LFortran still finalizes
    ! that copy, so the count is not checked here.
    finalized = 0

    select type (x)
    type is (item)
        n = f(x)
        if (n /= 8) error stop 7
        if (finalized /= 0) error stop 8
        n = x%get()
        if (n /= 8) error stop 9
        if (finalized /= 0) error stop 15
        call accept_base(x)
    end select
    if (finalized /= 0) error stop 16

    deallocate(x)
    if (finalized /= 1) error stop 4
end program
