program modulo_04
    implicit none
    real(8) :: a, p, r
    real :: x, y
    real(8), parameter :: big = 2.0_8**80
    real(8), parameter :: folded = modulo(big, 3.0_8)
    if (folded /= 1.0_8) error stop 11
    if (modulo(-big, 3.0_8) /= 2.0_8) error stop 12
    if (sign(1.0_8, modulo(6.0_8, -3.0_8)) /= -1.0_8) error stop 13
    a = 2.0_8**80
    p = 3.0_8
    if (mod(a, p) /= 1.0_8) error stop 1
    if (modulo(a, p) /= 1.0_8) error stop 2
    a = -a
    if (mod(a, p) /= -1.0_8) error stop 3
    if (modulo(a, p) /= 2.0_8) error stop 4
    p = -p
    if (modulo(a, p) /= -1.0_8) error stop 5
    a = -a
    if (modulo(a, p) /= -2.0_8) error stop 6
    a = -6.0_8
    p = 3.0_8
    r = modulo(a, p)
    if (r /= 0.0_8 .or. sign(1.0_8, r) /= 1.0_8) error stop 7
    a = 6.0_8
    p = -3.0_8
    r = modulo(a, p)
    if (r /= 0.0_8 .or. sign(1.0_8, r) /= -1.0_8) error stop 8
    x = 2.0**40
    y = 3.0
    if (mod(x, y) /= 1.0) error stop 9
    if (modulo(x, y) /= 1.0) error stop 10
end program
