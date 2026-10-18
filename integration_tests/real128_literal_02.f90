program real128_literal_02
    implicit none
    real(16) :: x
    x = 0.56477q-01
    if (kind(1.5Q2) /= 16) error stop 1
    if (x /= 0.56477e-01_16) error stop 2
    if (x == real(0.56477d-01, 16)) error stop 3
    if (1.5Q2 /= 150.0_16) error stop 4
end program
