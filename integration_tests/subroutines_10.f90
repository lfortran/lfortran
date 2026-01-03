program subroutines_10
    implicit none
    real :: x

    call subroutines_10_sub(x, 3, *100, *200)
    if (x < 0 .and. x > 10) error stop
    stop

    100 print*, "Negative input value"
    error stop

    200 print*, "Input value too large"
    error stop
end program subroutines_10

subroutine subroutines_10_sub(x, i, *, *)
    real, intent(out) :: x
    integer, intent(in) :: i
    if (i<0) return 1
    if (i>10) return 2
    x = i
end subroutine subroutines_10_sub
