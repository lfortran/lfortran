module bindc_optional_associate_01_m
    use iso_c_binding, only: c_int
    implicit none
    interface
        integer(c_int) function check(expected, x) bind(c, name="bindc_optional_associate_01_check")
            import :: c_int
            integer(c_int), value :: expected
            integer(c_int), optional, intent(in) :: x(*)
        end function
    end interface
contains
    subroutine forward(expected, x)
        integer(c_int), intent(in) :: expected
        integer(c_int), optional, intent(in) :: x(:)
        associate (flag => 0_c_int)
            block
                if (flag /= 0_c_int) error stop 2
                if (check(expected, x) /= 0_c_int) error stop 1
            end block
        end associate
    end subroutine
end module

program bindc_optional_associate_01
    use bindc_optional_associate_01_m
    implicit none
    integer(c_int) :: values(2)

    values = [3_c_int, 5_c_int]
    call forward(0_c_int)
    call forward(1_c_int, values)
    call forward(0_c_int)
end program
