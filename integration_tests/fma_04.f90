program fma_04
! Reductions `acc = acc + a*b` are left as a multiply and an add by the
! fma pass (`--fast`), so that LLVM can vectorize them; other `a*b + c`
! expressions are still fused.
implicit none
integer, parameter :: n = 100
real(4) :: x4(n), y4(n), z4(n), s4, t4
real(8) :: x8(n), y8(n), s8, t8, u8
integer :: i

do i = 1, n
    x4(i) = real(i, 4) / n
    y4(i) = real(n - i, 4) / n
    x8(i) = real(i, 8) / n
    y8(i) = real(n - i, 8) / n
end do

s4 = 0
do i = 1, n
    s4 = s4 + x4(i) * y4(i)
end do

s8 = 0
do i = 1, n
    s8 = x8(i) * y8(i) + s8
end do

t8 = 0
do i = 1, n
    t8 = t8 - x8(i) * y8(i)
end do

u8 = 0
do i = 1, n
    u8 = u8 + x8(i) * y8(i) + x8(i) * x8(i)
end do

do i = 1, n
    z4(i) = x4(i) * y4(i) + 1.0
end do
t4 = x4(n) * y4(1) + s4

! sum(i*(n-i))/n**2 = (n**2-1)/(6n), sum(i**2)/n**2 = (n+1)(2n+1)/(6n)
print *, s4, s8, t8, u8, t4
if (abs(s4 - 16.665_4) > 1e-4_4) error stop
if (abs(s8 - 16.665_8) > 1e-12_8) error stop
if (abs(t8 + 16.665_8) > 1e-12_8) error stop
if (abs(u8 - 50.5_8) > 1e-12_8) error stop
if (abs(t4 - (0.99_4 + s4)) > 1e-4_4) error stop
do i = 1, n
    if (abs(z4(i) - (1.0_4 + real(i*(n - i), 4) / n**2)) > 1e-6_4) error stop
end do
end program
