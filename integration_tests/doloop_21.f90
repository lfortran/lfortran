program doloop_21
! A constant step outside the default integer range must keep its sign
! and magnitude when the loop is lowered.
implicit none
integer(8) :: i, n, last

n = 0
last = 0
do i = 1_8, 10000000000_8, 3000000000_8
    n = n + 1
    last = i
end do
print *, n, last
if (n /= 4) error stop
if (last /= 9000000001_8) error stop

n = 0
last = 0
do i = 10000000000_8, 1_8, -3000000000_8
    n = n + 1
    last = i
end do
print *, n, last
if (n /= 4) error stop
if (last /= 1000000000_8) error stop
end program
