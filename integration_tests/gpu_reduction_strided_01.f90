program gpu_reduction_strided_01
! A reduction over more iterations than the threads its launch runs: each
! thread accumulates every stride-th iteration in its own slot. The values
! are small integers, so each sum is exact in single precision.
implicit none
integer, parameter :: n = 1000003
real, allocatable :: x(:), y(:)
real :: s, mx, mn, s_expected, mx_expected, mn_expected
integer :: i, j, cnt, odd, m, m_expected

allocate(x(n), y(n))
do i = 1, n
    x(i) = mod(i, 7) + 1
    y(i) = 0.5
end do

s = 0
do concurrent (i = 1:n) reduce(+:s)
    s = s + x(i)*y(i)
end do
s_expected = 0
do i = 1, n
    s_expected = s_expected + x(i)*y(i)
end do
if (s /= s_expected) error stop 1

! A reduction accumulates onto the value the scalar held before the loop.
s = 10
do concurrent (i = 1:n) reduce(+:s)
    s = s + 1
end do
if (s /= n + 10) error stop 2

mx = -huge(mx)
mn = huge(mn)
do concurrent (i = 1:n) reduce(max:mx) reduce(min:mn)
    mx = max(mx, x(i) + mod(i, 1009))
    mn = min(mn, x(i) - mod(i, 1013))
end do
mx_expected = -huge(mx)
mn_expected = huge(mn)
do i = 1, n
    mx_expected = max(mx_expected, x(i) + mod(i, 1009))
    mn_expected = min(mn_expected, x(i) - mod(i, 1013))
end do
if (mx /= mx_expected) error stop 3
if (mn /= mn_expected) error stop 4

! Two dimensions, two integer reductions, and fewer iterations than threads.
cnt = 0
m = 0
do concurrent (i = 1:1001, j = 1:999) reduce(+:cnt, m)
    cnt = cnt + 1
    m = m + mod(i*j, 7)
end do
m_expected = 0
do i = 1, 1001
    do j = 1, 999
        m_expected = m_expected + mod(i*j, 7)
    end do
end do
if (cnt /= 1001*999) error stop 5
if (m /= m_expected) error stop 6

cnt = 0
do concurrent (i = 1:5) reduce(+:cnt)
    cnt = cnt + i
end do
if (cnt /= 15) error stop 7

! A cycle of the offloaded loop ends the thread's iteration only.
odd = 0
do concurrent (i = 1:n) reduce(+:odd)
    if (mod(i, 2) == 0) cycle
    odd = odd + 1
end do
if (odd /= (n + 1)/2) error stop 8

print *, s, mx, mn, cnt, m, odd
end program
