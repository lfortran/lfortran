! The parallel_dispatch pass rejects this loop for Metal, which has no
! real(8): the driver reports the pass's error and prints nothing.
program show_pass_error_01
implicit none
integer, parameter :: n = 4
real(8) :: x(n)
integer :: i
do concurrent (i = 1:n)
    x(i) = real(i, 8)
end do
print *, x
end program
