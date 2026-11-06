module array_bounds_check_24_mod
    use iso_c_binding, only: c_float
    implicit none
contains
    subroutine bindc_assumed_size(n, a) bind(c)
        integer, intent(in) :: n
        real(c_float), intent(inout) :: a(n, *)
        a(n, 1) = 5.0_c_float
    end subroutine
end module

program array_bounds_check_24
    ! In strict mode a fixed-size actual argument is checked per dimension
    ! against a `bind(c)` assumed-size dummy's declared extent, so the size 2
    ! of the first dimension is rejected against the dummy's extent 3.
    use array_bounds_check_24_mod
    use iso_c_binding, only: c_float
    implicit none
    real(c_float) :: y(2, 1)
    y = 0.0_c_float
    call bindc_assumed_size(3, y)
    print *, "not reached"
end program
