module gpu_array_reduction_06_m
! The assignments right before a REDUCE of array elements that the kernel
! takes as initial values stay with the loop when device code runs it (acc,
! called from a `do concurrent`), and are taken only when the loop names the
! element assigned one way: s(k) with k = 1, or s(n) with the parameter
! n = 1, next to s(1) keep s(1) = 0 on the host. A loop that reduces a
! scalar and array elements together combines both.
implicit none
integer, parameter :: n = 1
contains
    pure subroutine acc(x, m, s)
        integer, intent(in) :: m
        real, intent(in) :: x(m)
        real, intent(inout) :: s(2)
        integer :: j
        s(1) = 0.0
        do concurrent (j = 1:m) reduce(+:s)
            s(1) = s(1) + x(j)
        end do
    end subroutine
end module

program gpu_array_reduction_06
use gpu_array_reduction_06_m
implicit none
integer, parameter :: m = 5000
real :: x(4), s(2), t(3), a(m), b(m)
integer :: i, k, total, c(3)
real :: rt, rs(2)
x = [1.0, 2.0, 3.0, 4.0]
s = 100.0
call acc(x, 4, s)
if (abs(s(1) - 10.0) > 1e-5 .or. s(2) /= 100.0) error stop 1
do concurrent (i = 1:3)
    block
        real :: u(2)
        u = 50.0
        call acc(x, 4, u)
        t(i) = u(1)
    end block
end do
if (any(abs(t - 10.0) > 1e-5)) error stop 2

a = 1.0
b = 2.0
k = 1
s = 77.0
s(1) = 0.0
do concurrent (i = 1:m) reduce(+:s)
    s(k) = s(k) + a(i)
    s(1) = s(1) + b(i)
end do
if (abs(s(1) - 3.0*m) > 1e-2 .or. s(2) /= 77.0) error stop 3

s = 77.0
s(1) = 0.0
do concurrent (i = 1:m) reduce(+:s)
    s(n) = s(n) + a(i)
    s(1) = s(1) + b(i)
end do
if (abs(s(1) - 3.0*m) > 1e-2 .or. s(2) /= 77.0) error stop 4

total = 5
c = 0
c(2) = 10
do concurrent (i = 1:m) reduce(+:total, c)
    total = total + 2
    c(1) = c(1) + 1
    c(2) = c(2) + mod(i, 3)
end do
if (total /= 5 + 2*m) error stop 5
if (c(1) /= m .or. c(2) /= 10 + 5001 .or. c(3) /= 0) error stop 6

rt = 1.0
rs = 0.0
rs(2) = 4.0
do concurrent (i = 1:m) reduce(+:rt, rs)
    rt = rt + 0.5
    rs(1) = rs(1) + 1.0
    rs(2) = rs(2) + 2.0
end do
if (rt /= 1.0 + 0.5*m .or. rs(1) /= m .or. rs(2) /= 4.0 + 2.0*m) error stop 7
print *, s(1), t(1), total, c, rt, rs
end program
