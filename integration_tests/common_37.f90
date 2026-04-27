program common_37
! Test COMMON block storage association with different array/scalar splits
! between program units (scalar+array vs array+scalar layout).
implicit none
integer :: s, a(2)
common /blk/ s, a
s = 1
a(1) = 2
a(2) = 3
call sub()
if (s /= 11) error stop
if (a(1) /= 22) error stop
if (a(2) /= 33) error stop
print *, s, a(1), a(2)
end program

subroutine sub()
implicit none
integer :: x(2), y
common /blk/ x, y
x(1) = 11
x(2) = 22
y = 33
end subroutine
