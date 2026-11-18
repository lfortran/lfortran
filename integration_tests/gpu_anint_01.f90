program gpu_anint_01
! anint and aint in a kernel round half away from zero and toward zero
! exactly, and convert to the kind of their result: anint(d, kind=4) of
! 2**24 + 1 is the real(4) 2**24, not a double.
implicit none
integer, parameter :: n = 8
double precision :: d(n)
real :: x(n), r(n), q(n), w(n), z(n)
integer :: i
d = [16777217d0, 2.5d0, -2.5d0, 0.25d0, -0.5d0, 1.5d0, &
    -16777217d0, 7.75d0]
x = real(d)
do concurrent (i = 1:n)
    r(i) = anint(d(i), kind=4) - 16777216.0
    q(i) = aint(d(i), kind=4) - 16777216.0
    w(i) = anint(x(i))
    z(i) = aint(x(i))
end do
print *, r
print *, w
print *, z
if (r(1) /= 0.0 .or. q(1) /= 0.0) error stop 1
if (r(2) /= 3.0 - 16777216.0 .or. r(3) /= -3.0 - 16777216.0) error stop 2
if (any(w /= [16777216.0, 3.0, -3.0, 0.0, -1.0, 2.0, -16777216.0, 8.0])) &
    error stop 3
if (any(z /= [16777216.0, 2.0, -2.0, 0.0, -0.0, 1.0, -16777216.0, 7.0])) &
    error stop 4
end program
