program gpu_nint_01
implicit none
real :: x(6)
double precision :: d(6)
integer :: r(6), i
integer(8) :: q(6)
x = [0.4, 0.5, 1.5, 2.5, -0.5, -2.7]
d = [0.4d0, 0.5d0, 1.5d0, 2.5d0, -0.5d0, -2.7d0]
do concurrent (i = 1:6)
    r(i) = nint(x(i))
    q(i) = nint(d(i), 8)
end do
print *, r
print *, q
if (any(r /= [0, 1, 2, 3, -1, -3])) error stop 1
if (any(q /= [0_8, 1_8, 2_8, 3_8, -1_8, -3_8])) error stop 2
end program
