module bindc_optional_array_01_m
    use iso_c_binding, only: c_int, c_int64_t
    implicit none
    interface
        subroutine chk(expect_present, md) bind(c, name='bindc_optional_array_01_check')
            import :: c_int, c_int64_t
            integer(c_int), value :: expect_present
            integer(c_int64_t), intent(in), optional :: md(*)
        end subroutine
        subroutine chk_scalar(expect_present, md) bind(c, name='bindc_optional_array_01_check_scalar')
            import :: c_int, c_int64_t
            integer(c_int), value :: expect_present
            integer(c_int64_t), intent(in), optional :: md
        end subroutine
    end interface
contains
    subroutine assumed_shape(expect_present, md)
        integer(c_int), intent(in) :: expect_present
        integer(c_int64_t), intent(in), optional :: md(:)
        call chk(expect_present, md)
    end subroutine

    subroutine explicit_shape(expect_present, md)
        integer(c_int), intent(in) :: expect_present
        integer(c_int64_t), intent(in), optional :: md(2)
        call chk(expect_present, md)
    end subroutine

    subroutine scalar(expect_present, md)
        integer(c_int), intent(in) :: expect_present
        integer(c_int64_t), intent(in), optional :: md
        call chk_scalar(expect_present, md)
    end subroutine

    subroutine forward_assumed_shape(expect_present, md)
        integer(c_int), intent(in) :: expect_present
        integer(c_int64_t), intent(in), optional :: md(:)
        call assumed_shape(expect_present, md)
    end subroutine
end module

program bindc_optional_array_01
    use bindc_optional_array_01_m
    implicit none
    integer(c_int64_t) :: v(2) = [7_c_int64_t, 8_c_int64_t]

    call assumed_shape(0_c_int)
    call assumed_shape(1_c_int, v)
    call explicit_shape(0_c_int)
    call explicit_shape(1_c_int, v)
    call scalar(0_c_int)
    call scalar(1_c_int, v(1))
    call forward_assumed_shape(0_c_int)
    call forward_assumed_shape(1_c_int, v)
    print *, "ok"
end program
