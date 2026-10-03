! A bind(c) procedure that contains internal procedures must keep its C ABI
! and binding label. Visiting the internal procedure used to reset the ABI of
! the host to Source, so the host lost its binding label and the C caller
! failed to link against it.
subroutine bindc_61_sub(x, y, z) bind(c, name="bindc_61_csub")
    use iso_c_binding, only: c_int, c_double
    implicit none
    integer(c_int), value :: x
    real(c_double), value :: y
    integer(c_int), intent(out) :: z
    z = 0
    call add(x)
    call add(int(y, c_int))
contains
    subroutine add(i)
        integer(c_int), intent(in) :: i
        z = z + i
    end subroutine
end subroutine

function bindc_61_fun(x) bind(c, name="bindc_61_cfun") result(r)
    use iso_c_binding, only: c_float
    implicit none
    real(c_float), value :: x
    real(c_float) :: r
    r = twice(x)
contains
    real(c_float) function twice(a)
        real(c_float), intent(in) :: a
        twice = 2 * a
    end function
end function

program bindc_61
    use iso_c_binding, only: c_int
    implicit none
    interface
        function call_from_c() bind(c, name="bindc_61_call_from_c") result(r)
            import :: c_int
            integer(c_int) :: r
        end function
    end interface
    integer(c_int) :: r
    r = call_from_c()
    print *, r
    if (r /= 14) error stop 1
    print *, "OK"
end program
