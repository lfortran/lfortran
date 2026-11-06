program gpu_rsqrt_01
! 1/sqrt(x) in a parallel loop, in single and double precision, with the
! integer 1 and the real 1 as numerator. With --fast a GPU may compute it as
! one reciprocal square root, which is not exact to the last bit.
implicit none
integer, parameter :: n = 1000
real :: x4(n), y4(n)
double precision :: x8(n), y8(n), z8(n)
integer :: i
do i = 1, n
    x4(i) = 0.5 + i
    x8(i) = 0.25d0 + 3*i
end do
do concurrent (i = 1:n)
    y4(i) = 1/sqrt(x4(i))
    y8(i) = 1.0d0/sqrt(x8(i))
    z8(i) = x8(i)*(1/sqrt(x8(i)))
end do
do i = 1, n
    if (abs(y4(i) - 1/sqrt(x4(i))) > 4*epsilon(1.0)*y4(i)) error stop "real(4)"
    if (abs(y8(i) - 1/sqrt(x8(i))) > 4*epsilon(1d0)*y8(i)) error stop "real(8)"
    if (abs(z8(i) - sqrt(x8(i))) > 8*epsilon(1d0)*z8(i)) error stop "x/sqrt(x)"
end do
print *, "ok"
end program
