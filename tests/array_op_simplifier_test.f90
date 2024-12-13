program main
implicit none

real :: a(5, 5), b(5, 5), c
real, allocatable :: d(:, :)

allocate(d(5, 5))
d = a + b + c

end program
