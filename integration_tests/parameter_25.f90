! Named constant arrays and repeated array constructors are read in place
! from one shared read-only global, without a stack copy (see the LLVM
! reference output).
program parameter_25
implicit none
real :: a(4, 2)
integer, parameter :: p(4) = [1, 2, 3, 4]
integer, parameter :: q(4) = [1, 2, 3, 4]
integer :: i
a(:, 1) = [1., 2., 3., 4.]
a(:, 2) = [1., 2., 3., 4.]
do i = 1, 4
    if (a(i, 1) /= real(p(i)) .or. a(i, 2) /= real(q(i))) error stop
end do
print *, "ok"
end program
