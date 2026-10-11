subroutine intrinsics_483_sub(r, s)
implicit none
real, intent(out) :: r, s
real :: x(2), y
x = 1
y = 1
r = sum(x)
s = sin(y)
end subroutine
