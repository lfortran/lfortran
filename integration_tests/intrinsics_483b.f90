subroutine intrinsics_483_sub(r, s, c)
implicit none
real, intent(out) :: r, s
character(:), allocatable, intent(out) :: c
real :: x(2), y
character(2) :: a
x = 1
y = 1
a = "ab"
r = sum(x)
s = sin(y)
c = a // "c"
end subroutine
