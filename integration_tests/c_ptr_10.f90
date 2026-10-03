program c_ptr_10
    use iso_c_binding, only: c_associated, c_loc, c_null_ptr, c_ptr
    implicit none

    integer, target :: x

    call check(c_null_ptr, .false.)
    call check(c_loc(x), .true.)

contains

    subroutine check(p, expected)
        type(c_ptr), intent(in) :: p
        logical, intent(in) :: expected

        if (c_associated(p) .neqv. expected) error stop
    end subroutine check

end program c_ptr_10
