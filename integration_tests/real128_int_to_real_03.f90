program real128_int_to_real_03
    ! Integer constants converted to real(real128) in constant expressions
    ! are folded at compile time, exactly (#15651).
    use, intrinsic :: iso_fortran_env, only: int64, real128
    implicit none
    real(real128), parameter :: threeqp = 3, thirdqp = 1/threeqp
    real(real128), parameter :: third = 1.0_real128/3.0_real128
    real(real128), parameter :: tol = 1e-30_real128
    real(real128), parameter :: a = 1/3.0_real128
    real(real128), parameter :: b = threeqp/3
    real(real128), parameter :: c = 1 + threeqp
    real(real128), parameter :: d = threeqp - 1
    real(real128), parameter :: e = 2*threeqp
    real(real128), parameter :: f = 1.0_real128/threeqp
    real(real128), parameter :: g = threeqp*threeqp
    real(real128), parameter :: h = -threeqp
    real(real128), parameter :: p = threeqp**2
    real(real128), parameter :: q = 1.0/threeqp
    real(real128), parameter :: r = 1_int64/3.0_real128
    real(real128), parameter :: big = 9007199254740993_int64
    real(real128) :: s
    parameter (s = 9007199254740993_int64)
    real(real128) :: x = 1/threeqp

    if (abs(thirdqp - third) > tol) error stop 1
    if (abs(a - third) > tol) error stop 2
    if (abs(b - 1) > tol) error stop 3
    if (abs(c - 4) > tol) error stop 4
    if (abs(d - 2) > tol) error stop 5
    if (abs(e - 6) > tol) error stop 6
    if (abs(f - third) > tol) error stop 7
    if (abs(g - 9) > tol) error stop 8
    if (abs(h + 3) > tol) error stop 9
    if (abs(p - 9) > tol) error stop 10
    if (abs(q - third) > tol) error stop 11
    if (abs(r - third) > tol) error stop 12
    if (big - 9007199254740992.0_real128 /= 1) error stop 13
    if (s - 9007199254740992.0_real128 /= 1) error stop 14
    if (abs(x - third) > tol) error stop 15
end program real128_int_to_real_03
