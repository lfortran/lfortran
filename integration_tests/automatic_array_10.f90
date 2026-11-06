module automatic_array_10_m
implicit none
contains
    ! Called on the host and in a DO CONCURRENT loop, which the GPU offload
    ! passes turn into device code: the host still fixes the bounds of `a`
    ! on entry, before `n` changes.
    pure subroutine work(n, a, s, grow)
        integer, intent(inout) :: n
        real, intent(inout) :: a(n)
        integer, intent(out) :: s
        logical, intent(in) :: grow
        if (grow) n = n + 10
        a = 1
        s = size(a)
    end subroutine
end module

program automatic_array_10
use automatic_array_10_m
implicit none
real :: x(3), y(4, 8)
integer :: n, s, i, k(8), ss(8)
n = 3
x = 0
s = 0
call work(n, x, s, .true.)
if (s /= 3) error stop 1
if (n /= 13) error stop 2
if (abs(sum(x) - 3) > 1e-6) error stop 3
k = 4
y = 0
ss = 0
do concurrent (i = 1:8)
    call work(k(i), y(:, i), ss(i), .false.)
end do
if (any(ss /= 4)) error stop 4
if (abs(sum(y) - 32) > 1e-6) error stop 5
print *, "ok"
end program
