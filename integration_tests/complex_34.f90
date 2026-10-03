program complex_34
    implicit none
    complex :: x(3)

    x(1)%re = 1
    x(2)%re = 2
    x(3)%re = 3

    x(1)%im = 4
    x(2)%im = 5
    x(3)%im = 6

    if (abs(real(x(1)) - 1.0) > 1e-5) error stop
    if (abs(real(x(2)) - 2.0) > 1e-5) error stop
    if (abs(real(x(3)) - 3.0) > 1e-5) error stop
    if (abs(aimag(x(1)) - 4.0) > 1e-5) error stop
    if (abs(aimag(x(2)) - 5.0) > 1e-5) error stop
    if (abs(aimag(x(3)) - 6.0) > 1e-5) error stop

    if (abs(x(1)%re - 1.0) > 1e-5) error stop
    if (abs(x(2)%re - 2.0) > 1e-5) error stop
    if (abs(x(3)%re - 3.0) > 1e-5) error stop
    if (abs(x(1)%im - 4.0) > 1e-5) error stop
    if (abs(x(2)%im - 5.0) > 1e-5) error stop
    if (abs(x(3)%im - 6.0) > 1e-5) error stop

    call foo(x%re)
    call foo(x%im)

contains

    subroutine foo(arr)
        real, intent(in) :: arr(3)
        if (abs(arr(1) + arr(2) + arr(3)) < 1e-5) error stop
    end subroutine
end program complex_34
