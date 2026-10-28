! Array expressions in IO lists for the C/C++ backends: fixed-size
! elementwise expressions are unrolled element by element.
program arrays_140
implicit none
real :: b(3)
real :: c(3)
integer :: i
b = 2.0
c = b + 1.0
do i = 1, 3
    if (abs(c(i) - 3.0) > 1e-6) error stop 1
end do
print *, b + 1.0
end program arrays_140
