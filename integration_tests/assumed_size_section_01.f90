subroutine copy3(x, y)
    implicit none
    real, intent(in) :: x(3)
    real, intent(out) :: y(3)

    y = x
end subroutine copy3

subroutine driver(a)
    implicit none
    real, intent(in) :: a(*)
    real :: y(3)

    call copy3(a(2:4), y)

    if (abs(y(1) - 2.0) > 1.0e-6) error stop 1
    if (abs(y(2) - 3.0) > 1.0e-6) error stop 1
    if (abs(y(3) - 4.0) > 1.0e-6) error stop 1
end subroutine driver

program main
    implicit none
    real :: a(5)

    a = [1.0, 2.0, 3.0, 4.0, 5.0]
    call driver(a)
    print *, "OK"
end program main
