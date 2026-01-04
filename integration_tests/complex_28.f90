! Test: complex*16 zero parameter with complex literal
program complex_28
    implicit none

    complex*16 zero
    parameter (zero = (0.0, 0.0))

    complex*16 t(2, 2)

    t(1, 1) = zero

    if (t(1, 1) /= zero) error stop
end program
