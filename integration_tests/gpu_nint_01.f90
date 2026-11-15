program gpu_nint_01
! nint of a real(4) value in a GPU kernel rounds half away from zero.
! The real(8) case, which Metal cannot offload, is gpu_nint_02.
implicit none
real :: x(6)
integer :: r(6), i
x = [0.4, 0.5, 1.5, 2.5, -0.5, -2.7]
do concurrent (i = 1:6)
    r(i) = nint(x(i))
end do
print *, r
if (any(r /= [0, 1, 2, 3, -1, -3])) error stop 1
end program
