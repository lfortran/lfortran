subroutine array_bounds_check_25_s(a)
    implicit none
    real, intent(inout) :: a(2, 2)
    a = 1.0
end subroutine

program array_bounds_check_25
    ! In strict mode an actual argument must match the dummy's shape exactly,
    ! so a same-total reshape is rejected (it is valid sequence association in
    ! the default mode).
    implicit none
    real :: x(4, 1)
    x = 0.0
    call array_bounds_check_25_s(x)
    print *, "not reached"
end program
