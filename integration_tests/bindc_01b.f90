module bindc_01b
use iso_c_binding, only: c_ptr, c_int
implicit none

interface
    subroutine ret_ptr(p) bind(c, name="ret_ptr_c")
    import :: c_ptr
    type(c_ptr), intent(out) :: p
    end subroutine

    ! same as `ret_ptr`, except, the only difference being "unspecified"
    ! intent of `p` in `ret_ptr_unspecified_intent`
    subroutine ret_ptr_unspecified_intent(p) bind(c, name="ret_ptr_c1")
    import :: c_ptr
    type(c_ptr) :: p
    end subroutine

    subroutine print_ptr(n, p) bind(c, name="print_ptr_c")
    import :: c_ptr, c_int
    integer(c_int), value, intent(in) :: n
    type(c_ptr), value, intent(in) :: p
    end subroutine
end interface

end module
