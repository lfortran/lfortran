! A bare NULL() passed by keyword takes its type and rank from the dummy
! argument the keyword names, not from an unrelated pointer in the caller.
module pointer_25_mod
    implicit none
    type :: uri
        integer :: n = 0
    end type
    type :: element_t
        integer :: m = 0
    end type
contains
    subroutine take_elem(a, q)
        integer, intent(in) :: a
        type(element_t), pointer, intent(in) :: q
        if (a /= 1) error stop 1
        if (associated(q)) error stop 2
    end subroutine

    subroutine take_scalar(a, base)
        integer, intent(in) :: a
        real, pointer, intent(in) :: base
        if (a /= 2) error stop 3
        if (associated(base)) error stop 4
    end subroutine

    subroutine take_int(a, base)
        integer, intent(in) :: a
        integer, pointer, intent(in) :: base
        if (a /= 42) error stop 5
        if (associated(base)) error stop 6
    end subroutine

    integer function count_present(a, x)
        integer, intent(in) :: a
        integer, intent(in), optional :: x
        count_present = a
        if (present(x)) count_present = count_present + 100
    end function
end module

program pointer_25
    use pointer_25_mod
    implicit none
    type(uri), pointer :: up
    real, pointer :: tmp(:)

    up => null()
    call take_elem(1, q=null())
    if (associated(up)) error stop 7

    tmp => null()
    call take_scalar(2, base=null())
    if (associated(tmp)) error stop 8

    call take_int(42, base=null())

    if (count_present(3, x=null()) /= 3) error stop 9
    print *, "ok"
end program
