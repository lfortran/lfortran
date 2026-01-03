program format_10
implicit none
integer :: n, i
integer, parameter :: grid_size = 3
real :: h(grid_size)
character(*), parameter :: fmt = '(i0,*(1x,es15.8e2))'
h = 1
n = 3
print fmt, n, h
do i = 1, grid_size
    if (abs(h(i) - 1.0) > 1e-9) error stop
end do
if (n /= 3) error stop
end program format_10
