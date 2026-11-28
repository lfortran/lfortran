module class_185_mod
    implicit none
    type :: slot
        integer :: id
    end type
contains
    subroutine mark_fixed(s)
        type(slot) :: s(5)
        if (s(2)%id /= 20) error stop 1
        s(4)%id = -s(4)%id
    end subroutine

    subroutine mark_sized(s, n)
        integer, intent(in) :: n
        type(slot) :: s(n)
        if (s(2)%id /= 20) error stop 2
        s(n)%id = -s(n)%id
    end subroutine

    subroutine mark_shaped(s)
        type(slot) :: s(:)
        if (size(s) /= 5) error stop 3
        if (s(2)%id /= 20) error stop 4
        s(1)%id = -s(1)%id
    end subroutine

    subroutine via_fixed_dummy(c)
        class(slot) :: c(5)
        call mark_fixed(c)
        call mark_sized(c, 5)
        call mark_shaped(c)
    end subroutine

    subroutine via_sized_dummy(c, n)
        integer, intent(in) :: n
        class(slot) :: c(n)
        call mark_fixed(c)
        call mark_sized(c, n)
        call mark_shaped(c)
    end subroutine
end module

program class_185
    use class_185_mod
    implicit none
    class(slot), allocatable :: rack(:)

    allocate(rack(5))
    rack%id = [10, 20, 30, 40, 50]
    call via_fixed_dummy(rack)
    if (any(rack%id /= [-10, 20, 30, -40, -50])) error stop 5
    call via_sized_dummy(rack, 5)
    if (any(rack%id /= [10, 20, 30, 40, 50])) error stop 6
    print *, "OK"
end program
