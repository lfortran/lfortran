program array_07_pack
    use iso_fortran_env, only: real64
    implicit none
    complex(real64) :: x(3)
    complex(real64) :: y(1)

    x = [(1.0_real64, 0.0_real64), (2.0_real64, 0.0_real64), (1.0_real64, 0.0_real64)]
    y = pack(x(2:), .not. x(2:) == x(1))
    if (abs(real(y(1), real64) - 2.0_real64) > 1.0e-12_real64) error stop 1
    if (abs(aimag(y(1))) > 1.0e-12_real64) error stop 2

    x = [(1.0_real64, 1.0_real64), (2.0_real64, 3.0_real64), (1.0_real64, 1.0_real64)]
    y = pack(x(2:), .not. x(2:) == x(1))
    if (abs(real(y(1), real64) - 2.0_real64) > 1.0e-12_real64) error stop 3
    if (abs(aimag(y(1)) - 3.0_real64) > 1.0e-12_real64) error stop 4
end program
