program real128_compare_01
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) :: a, b, c
    integer :: n

    ! Build the values from a quantity that is only known at run time so that
    ! the comparisons below are really executed instead of being folded away.
    n = command_argument_count()
    a = 1.0_real128 + real(n, real128)
    b = 2.0_real128 + real(n, real128)
    c = 1.0_real128 + real(n, real128)

    ! a < b
    if (.not. (a < b)) error stop 1
    if (.not. (a <= b)) error stop 2
    if (a > b) error stop 3
    if (a >= b) error stop 4

    ! b > a
    if (.not. (b > a)) error stop 5
    if (.not. (b >= a)) error stop 6
    if (b < a) error stop 7
    if (b <= a) error stop 8

    ! a == c
    if (.not. (a == c)) error stop 9
    if (a /= c) error stop 10
    if (.not. (a <= c)) error stop 11
    if (.not. (a >= c)) error stop 12
    if (a < c) error stop 13
    if (a > c) error stop 14

    ! a /= b
    if (a == b) error stop 15
    if (.not. (a /= b)) error stop 16

    ! negative operands reverse the ordering
    a = -a
    b = -b
    if (.not. (a > b)) error stop 17
    if (.not. (b < a)) error stop 18
    if (a < b) error stop 19
    if (b > a) error stop 20

    ! mixed signs
    a = -a
    if (.not. (a > b)) error stop 21
    if (.not. (b < a)) error stop 22
    if (a == b) error stop 23

    ! zero compares equal regardless of sign
    a = 0.0_real128 * real(n + 1, real128)
    b = -a
    if (.not. (a == b)) error stop 24
    if (a < b) error stop 25
    if (a > b) error stop 26

    print *, "ok"
end program real128_compare_01
