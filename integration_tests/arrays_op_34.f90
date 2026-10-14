! A named constant assigned to a contiguous array section is copied with a
! single memcpy instead of a loop (see the LLVM reference output).
program arrays_op_34
implicit none
real :: a(4, 3)
real, parameter :: c(4) = [1., 2., 3., 4.]
integer :: i
a = 0
i = 2
a(:, 1) = [1., 2., 3., 4.]
a(:, i) = c
a(2:3, 3) = [5., 6.]
if (a(1, 1) /= 1 .or. a(4, 1) /= 4 .or. a(1, 2) /= 1 .or. a(4, 2) /= 4) error stop
if (a(1, 3) /= 0 .or. a(2, 3) /= 5 .or. a(3, 3) /= 6 .or. a(4, 3) /= 0) error stop
print *, "ok"
end program
