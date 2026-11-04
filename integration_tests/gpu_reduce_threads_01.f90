program gpu_reduce_threads_01
! Reductions in parallel loops of iteration counts that do not divide into
! the threads of a launch, in one and two dimensions, with the accumulator
! starting from a value the loop adds to. Run with --gpu-reduction-threads=7
! as well, where every thread runs many iterations.
implicit none
integer, parameter :: dp = kind(0.d0), n = 1001, m = 37, p = 29
real(dp) :: a(n), b(n), c(m, p), s, mx, mn, prod
integer :: i, j, k
do i = 1, n
    a(i) = 1 + mod(i, 7)
    b(i) = 1.0_dp/(1 + mod(i, 5))
end do
do j = 1, p
    do i = 1, m
        c(i, j) = mod(3*i + j, 11) - 5
    end do
end do

s = 10
do concurrent (i = 1:n) reduce(+:s)
    s = s + a(i)*b(i)
end do
if (abs(s - (10 + sum(a*b))) > 1e-9_dp*abs(s)) error stop "sum"

mx = -huge(mx)
mn = huge(mn)
do concurrent (j = 1:p, i = 1:m) reduce(max:mx) reduce(min:mn)
    mx = max(mx, c(i, j)*(i + j))
    mn = min(mn, c(i, j)*(i - j))
end do
if (mx /= maxval(c*spread([(i, i = 1, m)], 2, p) &
        + c*spread([(j, j = 1, p)], 1, m))) error stop "max"
if (mn /= minval(c*spread([(i, i = 1, m)], 2, p) &
        - c*spread([(j, j = 1, p)], 1, m))) error stop "min"

prod = 1
do concurrent (i = 1:30) reduce(*:prod)
    prod = prod*(1 + 1.0_dp/i)
end do
if (abs(prod - 31) > 1e-12_dp*31) error stop "product"

k = 0
do concurrent (i = 1:n) reduce(+:k)
    k = k + mod(i, 3)
end do
if (k /= sum([(mod(i, 3), i = 1, n)])) error stop "integer sum"
print *, "ok"
end program
