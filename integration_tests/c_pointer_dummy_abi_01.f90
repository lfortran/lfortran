module c_pointer_dummy_abi_01_m
    implicit none
    type :: box
        integer, pointer :: items(:) => null()
    end type
contains
    subroutine retarget(p, target)
        integer, pointer, intent(inout) :: p(:)
        integer, target, intent(in) :: target(-2:)
        p => target
    end subroutine

    subroutine forward(p, target)
        integer, pointer, intent(inout) :: p(:)
        integer, target, intent(in) :: target(-2:)
        call retarget(p, target)
    end subroutine

    subroutine set_out(p, target)
        integer, pointer, intent(out) :: p(:)
        integer, target, intent(in) :: target(-2:)
        p => target
    end subroutine

    integer function first(p)
        integer, pointer, intent(in) :: p(:)
        first = p(-2)
    end function
end module

program c_pointer_dummy_abi_01
    use c_pointer_dummy_abi_01_m
    implicit none
    type(box) :: x
    integer, pointer :: p(:) => null()
    integer, target :: values(-2:0), other(-2:1)

    values(-2) = 11
    values(0) = 13
    other(-2) = 17
    other(1) = 23

    call retarget(x%items, values)
    if (.not. associated(x%items, values)) error stop 1
    if (lbound(x%items, 1) /= -2 .or. size(x%items) /= 3) error stop 2
    if (first(x%items) /= 11) error stop 3

    call forward(x%items, other)
    if (.not. associated(x%items, other)) error stop 4
    if (lbound(x%items, 1) /= -2 .or. size(x%items) /= 4) error stop 5
    if (x%items(1) /= 23 .or. values(-2) /= 11) error stop 6

    p => values
    call forward(p, other)
    if (.not. associated(p, other)) error stop 7
    if (lbound(p, 1) /= -2 .or. size(p) /= 4) error stop 8
    call retarget(p, values)
    if (.not. associated(p, values)) error stop 9
    if (p(0) /= 13 .or. other(-2) /= 17) error stop 10

    call set_out(p, other)
    call set_out(x%items, values)
    if (.not. associated(p, other)) error stop 11
    if (.not. associated(x%items, values)) error stop 12
    if (first(x%items) /= 11 .or. p(1) /= 23) error stop 13
end program
