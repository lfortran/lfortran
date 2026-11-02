module noalias_dummy_08_m
    use iso_c_binding, only: c_ptr, c_f_pointer, c_int
    implicit none
contains
    subroutine work(p, q, result)
        type(c_ptr), intent(in) :: p, q
        integer(c_int), intent(out) :: result
        integer(c_int), pointer :: a, b
        call c_f_pointer(p, a)
        call c_f_pointer(q, b)
        a = 1_c_int
        b = 2_c_int
        result = a
    end subroutine
end module

program noalias_dummy_08
    use iso_c_binding, only: c_loc, c_int
    use noalias_dummy_08_m, only: work
    implicit none
    integer(c_int), target :: x
    integer(c_int) :: result
    x = 0_c_int
    call work(c_loc(x), c_loc(x), result)
    if (result /= 2_c_int) error stop
end program
