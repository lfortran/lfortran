program block_01
implicit none
integer :: a, b, d
a = 1
b = 2
associate (n => a)
block
    real :: b, c
    b = 3
    c = -3
    d = 1
    ! TODO: Remove the previous line and fix the following line
    !d = cos(b + c)
end block
end associate
!print *, "b = ", b
!print *, "d = ", d
if (b /= 2) error stop
if (d /= 1) error stop
end