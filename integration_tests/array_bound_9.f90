module array_bound_9_mod
    ! Lower bounds of assumed-shape dummy arrays given by a derived-type
    ! component or an array element must be evaluated by value.
    implicit none

    type :: bounds_t
        integer :: first
        ! Unused: puts non-zero bytes right after `first`, so a wrong
        ! 8-byte load of the bound gives a bad value.
        integer :: next
    end type

contains

    subroutine component_lbound(items, bounds)
        type(bounds_t) :: bounds
        integer :: items(bounds%first:)

        if (lbound(items, 1) /= 1) error stop 1
        if (ubound(items, 1) /= 2) error stop 2
        if (items(1) /= 4) error stop 3
        if (items(2) /= 5) error stop 4
    end subroutine

    subroutine element_lbound(items, b)
        integer :: b(2)
        integer :: items(b(1):, b(2):)

        if (lbound(items, 1) /= -3) error stop 5
        if (lbound(items, 2) /= 7) error stop 6
        if (ubound(items, 2) /= 8) error stop 7
        if (items(-3, 7) /= 4) error stop 8
        if (items(-3, 8) /= 5) error stop 9
    end subroutine

end module

program array_bound_9
    use array_bound_9_mod
    implicit none

    integer :: items(2), items2(1, 2), b(2)
    type(bounds_t) :: bounds

    items = [4, 5]
    items2(1, :) = [4, 5]
    bounds%first = 1
    bounds%next = -1  ! never read; see the comment in bounds_t
    b = [-3, 7]

    call component_lbound(items, bounds)
    call element_lbound(items2, b)
    print *, "ok"
end program
