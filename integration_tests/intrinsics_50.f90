program intrinsics_50
implicit none

real :: x = 1.0
real, allocatable :: a(:, :)
real :: b(5, 5)
integer :: i, j
allocate(a(5, 10))

do i = lbound(a, 1), ubound(a, 1)
    do j = lbound(a, 2), ubound(a, 2)
        a(i, j) = i + j
    end do
end do

print *, sin([1., 2., 3., 4.])
print *, sin([[1., 2., 3.], [4., 5.]])
print *, sin([[1., 2., 3.], a, 5.])
print *, sin([[1., 2., b(:, 1)], a, 5.]) ! replace with allocatable a
print *, sin([[1., 2., x], a, 5.])

end program
