module gpu_integer_powers_01_m
    use, intrinsic :: iso_fortran_env, only: real32
    implicit none
contains
    pure elemental function half(x) result(y)
        real(real32), intent(in) :: x
        real(real32) :: y
        y = 0.5_real32*x
    end function
end module

program gpu_integer_powers_01
! Real powers with constant integer exponents 0 to 8 in device code are the
! products the host computes them with (gpu_integer_powers.cpp): of an
! element, of a function result and of an expression, which are evaluated
! once. Every value here is exact.
use, intrinsic :: iso_fortran_env, only: real32
use gpu_integer_powers_01_m, only: half
implicit none
integer, parameter :: n = 64
real(real32) :: x(n), p(0:8, n), q(n), s(n), ref
integer :: i, k
do i = 1, n
    x(i) = real(mod(i, 7) - 3, real32)*0.5_real32
end do
do concurrent (i = 1:n)
    p(0, i) = x(i)**0
    p(1, i) = x(i)**1
    p(2, i) = x(i)**2
    p(3, i) = x(i)**3
    p(4, i) = x(i)**4
    p(5, i) = x(i)**5
    p(6, i) = x(i)**6
    p(7, i) = x(i)**7
    p(8, i) = x(i)**8
    q(i) = half(x(i))**2 + 1.0_real32
    s(i) = (x(i) + 1.0_real32)**3
end do
do i = 1, n
    ref = 1.0_real32
    do k = 0, 8
        if (p(k, i) /= ref) error stop "x**k"
        ref = ref*x(i)
    end do
    if (q(i) /= (0.5_real32*x(i))*(0.5_real32*x(i)) + 1.0_real32) &
        error stop "half(x)**2"
    if (s(i) /= (x(i) + 1.0_real32)*(x(i) + 1.0_real32)*(x(i) + 1.0_real32)) &
        error stop "(x + 1)**3"
end do
print *, "ok"
end program
