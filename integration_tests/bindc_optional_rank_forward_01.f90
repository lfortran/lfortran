module bindc_optional_rank_forward_01_m
    use iso_c_binding, only: c_int
    implicit none
    interface
        integer(c_int) function check(expected, x) bind(c, name="bindc_optional_rank_forward_01_check")
            import :: c_int
            integer(c_int), value :: expected
            integer(c_int), optional, intent(in) :: x(..)
        end function
    end interface
contains
    subroutine forward(expected, x)
        integer(c_int), intent(in) :: expected
        integer(c_int), optional, intent(in) :: x(..)
        if (check(expected, x) /= 0_c_int) error stop 1
    end subroutine
end module

program bindc_optional_rank_forward_01
    use bindc_optional_rank_forward_01_m
    implicit none
    integer(c_int) :: scalar, a(2), b(2, 2), empty(0)

    scalar = 7_c_int
    a = [7_c_int, 8_c_int]
    b = reshape([7_c_int, 8_c_int, 9_c_int, 10_c_int], [2, 2])
    call forward(0_c_int)
    call forward(1_c_int, scalar)
    call forward(2_c_int, a)
    call forward(3_c_int, b)
    call forward(4_c_int, empty)
    call forward(0_c_int)
end program
