program gpu_nint_02
! nint of a real(8) value to integer(8) in a GPU kernel rounds half away
! from zero.
implicit none
double precision :: d(6)
integer(8) :: q(6)
integer :: i
d = [0.4d0, 0.5d0, 1.5d0, 2.5d0, -0.5d0, -2.7d0]
do concurrent (i = 1:6)
    q(i) = nint(d(i), 8)
end do
print *, q
if (any(q /= [0_8, 1_8, 2_8, 3_8, -1_8, -3_8])) error stop 2
end program
