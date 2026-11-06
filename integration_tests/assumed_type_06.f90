! c_loc of a type(*) dummy is the address of the actual argument
! (F2023 18.2.3.7), not the address of an internal wrapper.
module assumed_type_06_mod
    use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer, c_int64_t
    implicit none
contains

    subroutine write_first_assumed_size(a)
        type(*), target, intent(inout) :: a(*)
        call set_first(c_loc(a))
    end subroutine

    subroutine write_first_assumed_shape(a)
        type(*), target, intent(inout) :: a(:)
        call set_first(c_loc(a))
    end subroutine

    subroutine write_first_assumed_rank(a)
        type(*), target, intent(inout) :: a(..)
        call set_first(c_loc(a))
    end subroutine

    subroutine set_first(base)
        type(c_ptr), intent(in) :: base
        integer, pointer :: p
        call c_f_pointer(base, p)
        p = 42
    end subroutine

    integer(c_int64_t) function addr_of_scalar(buf)
        type(*), target :: buf
        addr_of_scalar = transfer(c_loc(buf), 0_c_int64_t)
    end function

end module

program assumed_type_06
    use assumed_type_06_mod
    implicit none
    integer, target :: v(3)
    integer, target :: s
    integer(c_int64_t) :: expect

    v = [1, 2, 3]
    call write_first_assumed_size(v)
    if (any(v /= [42, 2, 3])) error stop 1

    v = [1, 2, 3]
    call write_first_assumed_shape(v)
    if (any(v /= [42, 2, 3])) error stop 2

    v = [1, 2, 3]
    call write_first_assumed_rank(v)
    if (any(v /= [42, 2, 3])) error stop 3

    s = 7
    expect = transfer(c_loc(s), expect)
    if (addr_of_scalar(s) /= expect) error stop 4
    call write_first_assumed_rank(s)
    if (s /= 42) error stop 5

    print *, "ok"
end program
