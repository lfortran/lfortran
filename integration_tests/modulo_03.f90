module modulo_argument_values
    implicit none
    integer :: calls
contains
    function dividend() result(a)
        integer(8) :: a
        calls = calls + 1
        a = 9007199254740995_8
    end function

    function divisor() result(p)
        integer(8) :: p
        calls = calls + 1
        p = -3_8
    end function
end module

program modulo_03
    use modulo_argument_values
    implicit none
    integer(8) :: r, a, p
    calls = 0
    r = modulo(dividend(), 3_8)
    if (r /= 2_8) error stop 1
    if (calls /= 1) error stop 2
    calls = 0
    r = modulo(5_8, divisor())
    if (r /= -1_8) error stop 3
    if (calls /= 1) error stop 4
    a = -9007199254740993_8
    p = 2_8
    if (mod(a, p) /= -1_8) error stop 5
    if (modulo(a, p) /= 1_8) error stop 6
    p = -2_8
    if (modulo(a, p) /= -1_8) error stop 7
end program
