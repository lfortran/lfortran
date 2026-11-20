program class_pointer_target_01
    implicit none

    type :: base
        integer :: b = 1
    end type

    type, extends(base) :: child
        integer :: v = 7
    end type

    class(base), allocatable, target :: x
    type(child), target :: y
    class(*), allocatable :: u

    allocate(child :: x)
    select type (x)
    type is (child)
        x%v = 7
    end select
    y%v = 13

    allocate(child :: u)
    select type (u)
    type is (child)
        u%v = 29
    end select

    ! Direct target actuals to pointer dummy
    call check_type_is(x, 7)
    call check_type_is(y, 13)

    ! class(base), allocatable actual through class(base), target, intent(in/inout) dummy
    call mid_in(x, 7)
    call mid_inout(x, 7)

    ! type(child), target actual through class(base), target, intent(in/inout) dummy
    call mid_in(y, 13)
    call mid_inout(y, 13)

    ! Verify mutation through pointer dummy associated with intent(inout) target dummy
    call mid_mutate(y, 42)
    if (y%v /= 42) error stop 1

    ! Unlimited polymorphic target dummy to unlimited polymorphic pointer dummy
    call mid_upoly(u, 29)

contains

    subroutine check_type_is(p, expected)
        class(base), pointer, intent(in) :: p
        integer, intent(in) :: expected
        if (.not. associated(p)) error stop 2
        select type (p)
        type is (child)
            if (p%v /= expected) error stop 3
        class default
            error stop 4
        end select
    end subroutine

    subroutine check_class_is(p, expected)
        class(base), pointer, intent(in) :: p
        integer, intent(in) :: expected
        if (.not. associated(p)) error stop 5
        select type (p)
        class is (child)
            if (p%v /= expected) error stop 6
        class default
            error stop 7
        end select
    end subroutine

    subroutine set_val(p, val)
        class(base), pointer, intent(in) :: p
        integer, intent(in) :: val
        select type (p)
        type is (child)
            p%v = val
        class default
            error stop 8
        end select
    end subroutine

    subroutine check_upoly(p, expected)
        class(*), pointer, intent(in) :: p
        integer, intent(in) :: expected
        if (.not. associated(p)) error stop 9
        select type (p)
        type is (child)
            if (p%v /= expected) error stop 10
        class default
            error stop 11
        end select
    end subroutine

    subroutine mid_in(a, expected)
        class(base), target, intent(in) :: a
        integer, intent(in) :: expected
        call check_type_is(a, expected)
        call check_class_is(a, expected)
    end subroutine

    subroutine mid_inout(a, expected)
        class(base), target, intent(inout) :: a
        integer, intent(in) :: expected
        call check_type_is(a, expected)
        call check_class_is(a, expected)
    end subroutine

    subroutine mid_mutate(a, val)
        class(base), target, intent(inout) :: a
        integer, intent(in) :: val
        call set_val(a, val)
    end subroutine

    subroutine mid_upoly(a, expected)
        class(*), target, intent(in) :: a
        integer, intent(in) :: expected
        call check_upoly(a, expected)
    end subroutine

end program
